module High : sig
  type t
  type 'a s = 'a Lwt.t

  external inj : 'a s -> ('a, t) Tar.io = "%identity"
  external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a Lwt.t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

let value v = Tar.High (High.inj v)

let ok_value v = value (Lwt_result.ok v)

let run t stream =
  let rec run : type a. (a, 'err, High.t) Tar.t -> (a, 'err) result Lwt.t =
    function
    | Tar.Write str ->
      (* Can this not fail?!? Obviously, it can, but we never know?? *)
      Lwt_result.ok (Dream.write stream str)
    | Tar.Seek _ | Tar.Read _ | Tar.Really_read _ -> assert false
    | Tar.Return value -> Lwt.return value
    | Tar.High value -> High.prj value
    | Tar.Bind (x, f) ->
      let open Lwt_result.Syntax in
      let* v = run x in
      run (f v)
  in
  run t

let header_of_file mod_time (file : Builder_db.file) =
  let file_mode = if Fpath.is_prefix Fpath.(v "bin/") file.filepath then
      0o755
    else
      0o644
  in
  Tar.Header.make ~file_mode ~mod_time (Fpath.to_string file.filepath) (Int64.of_int file.size)

let contents datadir file : unit -> (string option, _, _) Tar.t =
  let state = ref `Initial in
  let dispenser () =
    let open Tar.Syntax in
    let src = Fpath.append datadir (Model.artifact_path file) in
    let* state' =
      match !state with
      | `Initial ->
        let* fd = ok_value (Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string src)) in
        let s = `Active fd in
        state := s; Tar.return (Ok s)
      | `Active _ | `Closed as s -> Tar.return (Ok s)
    in
    match state' with
    | `Closed -> Tar.return (Ok None)
    | `Active fd ->
      let* data = ok_value (Lwt_io.read ~count:65536 fd) in
      if String.length data = 0 then begin
        state := `Closed;
        let* () = ok_value (Lwt_io.close fd) in
        Tar.return (Ok None)
      end else
        Tar.return (Ok (Some data))
  in
  dispenser

let entries datadir finish files =
  let files =
    List.map (fun file ->
        let hdr = header_of_file finish file in
        let level = Some Tar.Header.Posix in
        (level, hdr, contents datadir file)
      )
      files
  in
  let files = ref files in
  fun () -> match !files with
    | [] -> Tar.return (Ok None)
    | f :: fs -> files := fs; Tar.return (Ok (Some f))

let targz_response datadir finish files stream =
  let entries : (_, _) Tar.entries = entries datadir finish files in
  let global_hdr =
    Tar.Header.Extended.make
      ~comment:"Tar file produced by builder-web.%%VERSION_NUM%%"
      ()
  in
  let finish32 = Int64.to_int32 finish in
  Logs.err (fun m -> m "finished at %ld (%Ld)" finish32 finish);
  run (Tar_gz.out_gzipped ~level:9 ~mtime:finish32 Gz.Unix (Tar.out ~global_hdr entries)) stream

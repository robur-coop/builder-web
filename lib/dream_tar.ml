open Lwt.Infix

module Writer = struct
  type out_channel =
    { mutable gz : Gz.Def.encoder
    ; ic : Cstruct.t
    ; oc : Cstruct.t
    ; stream : Dream.stream }

  type 'a t = 'a Lwt.t

  let really_write ({ oc; stream; _ } as state) cs =
    let rec until_await gz =
      match Gz.Def.encode gz with
      | `Await gz -> state.gz <- gz ; Lwt.return_unit
      | `Flush gz ->
        let max = Cstruct.length oc - Gz.Def.dst_rem gz in
        let str = Cstruct.to_string ~len:max oc in
        Dream.write stream str >>= fun () ->
        let { Cstruct.buffer; off= cs_off; len= cs_len; } = oc in
        until_await (Gz.Def.dst gz buffer cs_off cs_len)
      | `End _gz -> assert false in
    if Cstruct.length cs = 0
    then Lwt.return_unit
    else ( let { Cstruct.buffer; off; len; } = cs in
           let gz = Gz.Def.src state.gz buffer off len in
           until_await gz )
end

module HW = Tar.HeaderWriter(Lwt)(Writer)

let write_block (header : Tar.Header.t) lpath ({ Writer.ic= buf; _ } as state) =
  HW.write ~level:Tar.Header.Ustar header state >>= fun () ->
  Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string lpath) >>= fun ic ->
  let rec loop () =
    let { Cstruct.buffer; off; len; } = buf in
    Lwt_io.read_into_bigstring ic buffer off len >>= function
    | 0 -> Lwt.return ()
    | len' ->
      Writer.really_write state (Cstruct.sub buf 0 len') >>= fun () ->
      loop ()
  in
  loop () >>= fun () ->
  Writer.really_write state (Tar.Header.zero_padding header)

let header_of_file mod_time (file : Builder_db.file) =
  let file_mode = if Fpath.is_prefix Fpath.(v "bin/") file.filepath then
      0o755
    else
      0o644
  in
  Tar.Header.make ~file_mode ~mod_time (Fpath.to_string file.filepath) (Int64.of_int file.size)

let targz_response datadir finish (files : Builder_db.file list) (stream : Dream.stream) =
  let state =
    let ic = Cstruct.create (4 * 4 * 1024) in
    let oc = Cstruct.create 4096 in
    let gz =
      let w = De.Lz77.make_window ~bits:15 in
      let q = De.Queue.create 0x1000 in
      let mtime = Int32.of_float (Unix.gettimeofday ()) in
      let gz = Gz.Def.encoder `Manual `Manual ~mtime Gz.Unix ~q ~w ~level:4 in
      let { Cstruct.buffer; off; len; } = oc in
      Gz.Def.dst gz buffer off len
    in
    { Writer.gz; ic; oc; stream; }
  in
  Lwt_list.iter_s (fun file ->
      let hdr = header_of_file finish file in
      write_block hdr Fpath.(datadir // file.localpath) state)
    files >>= fun () ->
  Writer.really_write state Tar.Header.zero_block >>= fun () ->
  Writer.really_write state Tar.Header.zero_block >>= fun () ->
  (* assert (Gz.Def.encode gz = `Await) *)
  let rec until_end gz = match Gz.Def.encode gz with
    | `Await _gz -> assert false
    | `Flush gz | `End gz as flush_or_end ->
      let max = Cstruct.length state.oc - Gz.Def.dst_rem gz in
      let str = Cstruct.to_string ~len:max state.oc in
      Dream.write stream str >>= fun () -> match flush_or_end with
      | `Flush gz ->
        let { Cstruct.buffer; off= cs_off; len= cs_len; } = state.oc in
        until_end (Gz.Def.dst gz buffer cs_off cs_len)
      | `End _ -> Lwt.return_unit
  in
  until_end (Gz.Def.src state.gz De.bigstring_empty 0 0) >>= fun () ->
  Dream.flush stream >>= fun () ->
  Dream.close stream

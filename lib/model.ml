open Lwt_result.Infix

let src = Logs.Src.create "builder-web.model" ~doc:"Builder_web model"
module Log = (val Logs.src_log  src : Logs.LOG)

module type CONN = Caqti_lwt.CONNECTION

type error = [ Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t | `Msg of string ]

let pp_error ppf = function
  | `Not_found -> Format.fprintf ppf "value not found in database"
  | `File_error path -> Format.fprintf ppf "error reading file %a" Fpath.pp path
  | `Msg e -> Format.fprintf ppf "error %s" e
  | #Caqti_error.call_or_retrieve as e ->
    Caqti_error.pp ppf e

let not_found = function
  | None -> Lwt.return (Error `Not_found :> (_, [> error ]) result)
  | Some v -> Lwt_result.return v

let staging datadir = Fpath.(datadir / "_staging")

let read_file filepath =
  Lwt.try_bind
    (fun () -> Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string filepath))
    (fun ic -> Lwt_result.ok (Lwt_io.read ic))
    (function
      | Unix.Unix_error (e, _, _) ->
        Logs.warn (fun m -> m "Error reading local file %a: %s"
                      Fpath.pp filepath (Unix.error_message e));
        Lwt.return_error (`File_error filepath)
      | e -> Lwt.fail e)

let build_artifact build filepath (module Db : CONN) =
  Db.find_opt Builder_db.Build_artifact.get_by_build_uuid (build, filepath)
  >>= function
  | Some (_id, file) ->
    read_file file.Builder_db.localpath >|= fun data -> data, file.Builder_db.sha256
  | None ->
    Lwt.return_error `Not_found

let build_artifacts build (module Db : CONN) =
  Db.collect_list Builder_db.Build_artifact.get_all_by_build build >|=
  List.map snd

let build uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>=
  not_found

let build_meta job (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest job >|=
  Option.map (fun (_id, meta, file) -> (meta, file))

let build_hash hash (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_hash hash

let build_exists uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >|=
  Option.is_some

let main_binary id main_binary (module Db : CONN) =
  match main_binary with
  | None -> Lwt_result.return None
  | Some main_binary ->
    Db.find Builder_db.Build_artifact.get_by_build (id, main_binary) >|= fun (_id, file) ->
    Some file

let job job (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_all_meta_by_name job >|=
  List.map (fun (_id, meta, main_binary) -> (meta, main_binary))

let jobs (module Db : CONN) =
  Db.collect_list Builder_db.Job.get_all ()

let user username (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >|=
  Option.map snd

let cleanup_staging datadir (module Db : Caqti_lwt.CONNECTION) =
  let cleanup_staged staged =
    match Uuidm.of_string (Fpath.to_string staged) with
    | None ->
      Log.warn (fun m -> m "Non-uuid staged files: %a" Fpath.pp
                   Fpath.(staging datadir // staged));
      Lwt.return (Bos.OS.Path.delete ~recurse:true Fpath.(staging datadir // staged))
    | Some uuid ->
      let staged = Fpath.(staging datadir // staged) in
      Db.find_opt Builder_db.Build.get_by_uuid uuid >>= function
      | Some (_id, build) ->
        Db.find Builder_db.Job.get build.job_id >>= fun job_name ->
        let destdir = Fpath.(datadir / job_name / Uuidm.to_string uuid) in
        Lwt.return (Bos.OS.Path.move staged destdir)
      | None ->
        Lwt.return (Bos.OS.Path.delete ~recurse:true Fpath.(staging datadir // staged))
  in
  Lwt.return (Bos.OS.Dir.contents ~rel:true (staging datadir)) >>= fun stageds ->
  Lwt_result.ok @@
  Lwt_list.iter_s
    (fun staged ->
       Lwt.map (function
       | Error (_ as e) ->
         Log.warn (fun m -> m "Failed cleaning up staged files %a in %a: %a"
                      Fpath.pp staged
                      Fpath.pp Fpath.(staging datadir // staged)
                      pp_error e)
       | Ok () -> ())
       (cleanup_staged staged))
    stageds

let save file data =
  let open Lwt.Infix in
  Lwt.catch
    (fun () ->
       Lwt_io.open_file ~mode:Lwt_io.Output (Fpath.to_string file) >>= fun oc ->
       Lwt_io.write oc data >>= fun () ->
       Lwt_io.close oc
       |> Lwt_result.ok)
    (function
      | Unix.Unix_error (e, _, _) ->
          Lwt_result.fail (`Msg (Unix.error_message e))
      | e -> Lwt.fail e)

let save_exec build_dir exec =
  let cs = Builder.Asn.exec_to_cs exec in
  save Fpath.(build_dir / "full") (Cstruct.to_string cs)

let save_file dir staging (filepath, data) =
  let size = String.length data in
  let sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
  let localpath = Fpath.append dir filepath in
  let destpath = Fpath.append staging filepath in
  Lwt_result.lift (Bos.OS.Dir.create (Fpath.parent destpath)) >>= fun _ ->
  save destpath data >|= fun () ->
  { Builder_db.filepath; localpath; sha256; size }

let save_files dir staging files =
  List.fold_left
    (fun r file ->
       r >>= fun acc ->
       save_file dir staging file >>= fun file ->
       Lwt_result.return (file :: acc))
    (Lwt_result.return [])
    files

let save_all basedir staging_dir ((job, uuid, _, _, _, _, artifacts) as exec) =
  let build_dir = Fpath.(basedir / job.Builder.name / Uuidm.to_string uuid) in
  let input_dir = Fpath.(build_dir / "input")
  and staging_input_dir = Fpath.(staging_dir / "input") in
  let output_dir = Fpath.(build_dir / "output")
  and staging_output_dir = Fpath.(staging_dir / "output") in
  Lwt.return (Bos.OS.Dir.create staging_dir) >>= (fun created ->
      if not created
      then Lwt_result.fail (`Msg "build directory already exists")
      else Lwt_result.return ()) >>= fun () ->
  Lwt.return (Bos.OS.Dir.create staging_input_dir) >>= fun _ ->
  Lwt.return (Bos.OS.Dir.create staging_output_dir) >>= fun _ ->
  save_exec staging_dir exec >>= fun () ->
  save_files output_dir staging_output_dir artifacts >>= fun artifacts ->
  save_files input_dir staging_input_dir job.Builder.files >>= fun input_files ->
  Lwt_result.return (artifacts, input_files)

let commit_files basedir staging_dir job_name uuid =
  let job_dir = Fpath.(basedir / job_name) in
  let dest = Fpath.(job_dir / Uuidm.to_string uuid) in
  Lwt.return (Bos.OS.Dir.create job_dir) >>= fun _ ->
  Lwt.return (Bos.OS.Path.move staging_dir dest)

let add_build
    basedir
    ((job, uuid, console, start, finish, result, _) as exec)
    (module Db : CONN) =
  let open Builder_db in
  let job_name = job.Builder.name in
  let staging_dir = Fpath.(staging basedir / Uuidm.to_string uuid) in
  let or_cleanup x =
    Lwt_result.map_err (fun e ->
        Bos.OS.Dir.delete ~recurse:true staging_dir
        |> Result.iter_error (fun e ->
            Log.err (fun m -> m "Failed to remove staging dir %a: %a"
                        Fpath.pp staging_dir
                        pp_error e));
        e)
      x
  in
  or_cleanup (save_all basedir staging_dir exec) >>= fun (artifacts, input_files) ->
  let main_binary =
    match List.find_all
            (fun file ->
               Fpath.is_prefix
                 (Fpath.v "bin/")
                 file.Builder_db.filepath)
            artifacts with
    | [ main_binary ] -> Some main_binary.filepath
    | [] ->
      Log.debug (fun m -> m "Zero binaries for build %a" Uuidm.pp uuid);
      None
    | binaries ->
      Log.debug (fun m -> m "Multiple binaries for build %a: %a" Uuidm.pp uuid
                    Fmt.(list ~sep:(any ",") Fpath.pp)
                    (List.map (fun f -> f.filepath) binaries));
      None
  in
  let r =
    Db.start () >>= fun () ->
    Db.exec Job.try_add job_name >>= fun () ->
    Db.find Job.get_id_by_name job_name >>= fun job_id ->
    Db.exec Build.add { Build.uuid; start; finish; result;
                        console; script = job.Builder.script;
                        main_binary; job_id } >>= fun () ->
    Db.find last_insert_rowid () >>= fun id ->
    List.fold_left
      (fun r file ->
         r >>= fun () ->
         Db.exec Build_artifact.add (file, id))
      (Lwt_result.return ())
      artifacts >>= fun () ->
    List.fold_left
      (fun r file ->
         r >>= fun () ->
         Db.exec Build_file.add (file, id))
      (Lwt_result.return ())
      input_files >>= fun () ->
    Db.commit () >>= fun () ->
    commit_files basedir staging_dir job_name uuid
  in
  or_cleanup r

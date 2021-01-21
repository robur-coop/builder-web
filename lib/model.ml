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
  | Some (localpath, sha256) ->
    read_file localpath >|= fun data -> data, sha256
  | None ->
    Lwt.return_error `Not_found

let build_artifacts build (module Db : CONN) =
  Db.collect_list Builder_db.Build_artifact.get_all_by_build build >|=
  List.map snd

let build uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>=
  not_found

let job job (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_all_meta_by_name job

let jobs (module Db : CONN) =
  Db.collect_list Builder_db.Job.get_all () >|=
  List.map snd

let user username (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >|=
  Option.map snd

let save file data =
  let open Lwt.Infix in
  Lwt_io.open_file ~mode:Lwt_io.Output (Fpath.to_string file) >>= fun oc ->
  Lwt_io.write oc data >>= fun () ->
  Lwt_io.close oc

let save_exec build_dir exec =
  let cs = Builder.Asn.exec_to_cs exec in
  save Fpath.(build_dir / "full") (Cstruct.to_string cs)

let save_file dir (filepath, data) =
  let sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
  let localpath = Fpath.append dir filepath in
  Lwt.bind
    (save localpath data)
    (fun () -> Lwt.return { Builder_db.filepath; localpath; sha256 })

let save_all basedir ((job, uuid, _, _, _, _, artifacts) as exec) =
  let build_dir = Fpath.(basedir / job.Builder.name / Uuidm.to_string uuid) in
  let input_dir = Fpath.(build_dir / "input") in
  let output_dir = Fpath.(build_dir / "output") in
  Lwt.return (Bos.OS.Dir.create build_dir) >>= (fun created ->
      if not created
      then Lwt_result.fail (`Msg "build directory already exists")
      else Lwt_result.return ()) >>= fun () ->
  Lwt.return (Bos.OS.Dir.create input_dir) >>= fun _ ->
  Lwt.return (Bos.OS.Dir.create output_dir) >>= fun _ ->
  Lwt.catch
    (fun () ->
       let open Lwt.Infix in
       save_exec build_dir exec >>= fun () ->
       Lwt_list.map_s (save_file input_dir) job.Builder.files >>= fun input_files ->
       Lwt_list.map_s (save_file output_dir) artifacts >>= fun artifacts ->
       Lwt_result.return (artifacts, input_files))
    (function
      | Unix.Unix_error (e, _, _) ->
          Lwt_result.fail (`Msg (Unix.error_message e))
      | e -> Lwt.fail e)

let add_build
    basedir
    ((job, uuid, console, start, finish, result, _) as exec)
    (module Db : CONN) =
  let open Builder_db in
  let job_name = job.Builder.name in
  save_all basedir exec >>= fun (artifacts, input_files) ->
  Db.exec Job.try_add job_name >>= fun () ->
  Db.find Job.get_id_by_name job_name >>= fun job_id ->
  Db.exec Build.add { Build.uuid; start; finish; result;
                      console; script = job.Builder.script; job_id } >>= fun () ->
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
    input_files

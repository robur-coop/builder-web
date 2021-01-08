open Lwt_result.Infix

let src = Logs.Src.create "builder-web.model" ~doc:"Builder_web model"
module Log = (val Logs.src_log  src : Logs.LOG)

module type CONN = Caqti_lwt.CONNECTION

type error = [ Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t ]

let pp_error ppf = function
  | `Not_found -> Format.fprintf ppf "value not found in database"
  | `File_error path -> Format.fprintf ppf "error reading file %a" Fpath.pp path
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

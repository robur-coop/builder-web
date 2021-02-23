let old_user_version = 2L
let new_user_version = 3L

let set_version version =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    (Printf.sprintf "PRAGMA user_version = %Ld" version)

let alter_build_artifact =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "ALTER TABLE build_artifact ADD COLUMN size INTEGER NOT NULL"

let alter_build_file =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "ALTER TABLE build_file ADD COLUMN size INTEGER NOT NULL"

let collect_build_artifact_localpath =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    Caqti_type.(tup2 int64 string)
    "SELECT id, localpath FROM build_artifact"

let collect_build_file_localpath =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    Caqti_type.(tup2 int64 string)
    "SELECT id, localpath FROM build_file"

let set_build_artifact_size =
  Caqti_request.exec ~oneshot:true
    Caqti_type.(tup2 int64 int64)
    "UPDATE build_artifact SET size = ?2 WHERE id = ?1"

let set_build_file_size =
  Caqti_request.exec ~oneshot:true
    Caqti_type.(tup2 int64 int64)
    "UPDATE build_file SET size = ?2 WHERE id = ?1"

let migrate (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Db.find Builder_db.get_application_id () >>= fun application_id ->
  Db.find Builder_db.get_version () >>= fun user_version ->
  if application_id <> Builder_db.application_id || user_version <> old_user_version
  then Error (`Wrong_version (application_id, user_version))
  else
    Db.exec alter_build_artifact () >>= fun () ->
    Db.iter_s collect_build_artifact_localpath
      (fun (id, localpath) ->
         let stats = Unix.stat localpath in
         Db.exec set_build_artifact_size (id, Int64.of_int stats.st_size))
      ()
    >>= fun () ->
    Db.exec alter_build_file () >>= fun () ->
    Db.iter_s collect_build_file_localpath
      (fun (id, localpath) ->
         let stats = Unix.stat localpath in
         Db.exec set_build_file_size (id, Int64.of_int stats.st_size))
      ()

(* FIXME: rollback. Requires copying data and creating new table without size column. *)

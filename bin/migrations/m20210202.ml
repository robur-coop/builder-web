open Rresult.R.Infix

let migrate (module Db : Caqti_blocking.CONNECTION) =
  let job_build_idx =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "CREATE INDEX job_build_idx ON build(job)";
  in
  Db.find Builder_db.get_application_id () >>= fun application_id ->
  Db.find Builder_db.get_version () >>= fun user_version ->
  if application_id <> Builder_db.application_id || user_version <> 1L
  then Error (`Wrong_version (application_id, user_version))
  else Db.exec job_build_idx ()

let rollback (module Db : Caqti_blocking.CONNECTION) =
  let q =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "DROP INDEX IF EXISTS job_build_idx"
  in
  Db.find Builder_db.get_application_id () >>= fun application_id ->
  Db.find Builder_db.get_version () >>= fun user_version ->
  if application_id <> Builder_db.application_id || user_version <> 1L
  then Error (`Wrong_version (application_id, user_version))
  else
    Db.exec q ()

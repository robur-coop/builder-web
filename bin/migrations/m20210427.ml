let old_version = 3L and new_version = 3L
let identifier = "2021-04-27"
let migrate_doc = "add index idx_build_job_start on build"
let rollback_doc = "rollback index idx_build_job_start on build"

open Grej.Infix

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  let idx_build_job_start =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)"
  in
  let rm_job_build_idx =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "DROP INDEX IF EXISTS job_build_idx"
  in
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec rm_job_build_idx () >>= fun () ->
  Db.exec idx_build_job_start ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  let job_build_idx =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "CREATE INDEX job_build_idx ON build(job)"
  in
  let rm_idx_build_job_start =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "DROP INDEX IF EXISTS idx_build_job_start"
  in
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec rm_idx_build_job_start () >>= fun () ->
  Db.exec job_build_idx ()

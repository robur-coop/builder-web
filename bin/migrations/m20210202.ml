let old_version = 1L and new_version = 1L
let identifier = "2021-02-02"
let migrate_doc = "add index job_build_idx on build"
let rollback_doc = "rollback index job_build_idx on build"

open Rresult.R.Infix

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  let job_build_idx =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "CREATE INDEX job_build_idx ON build(job)";
  in
  Grej.check_version ~user_version:1L (module Db) >>= fun () ->
  Db.exec job_build_idx ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  let q =
    Caqti_request.exec ~oneshot:true
      Caqti_type.unit
      "DROP INDEX IF EXISTS job_build_idx"
  in
  Grej.check_version ~user_version:1L (module Db) >>= fun () ->
  Db.exec q ()

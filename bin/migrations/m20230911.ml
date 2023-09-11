let new_version = 17L and old_version = 16L
and identifier = "2023-09-11"
and migrate_doc = "index failed builds on main binary is null"
and rollback_doc = "index failed builds on exit code"

open Grej.Syntax

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:old_version (module Db) in
  let* () =
    Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
             "CREATE INDEX idx_build_failed ON build(job, start_d DESC, start_ps DESC) \
             WHERE main_binary IS NULL")
      ()
  in
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:new_version (module Db) in
  let* () =
    Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
             "CREATE INDEX idx_build_failed ON build(job, start_d DESC, start_ps DESC) \
              WHERE result_code <> 0")
      ()
  in
  Db.exec (Grej.set_version old_version) ()

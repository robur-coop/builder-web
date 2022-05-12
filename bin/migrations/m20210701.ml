let new_version = 11L and old_version = 10L
let identifier = "2021-07-01"
let migrate_doc = "build.main_binary deferred foreign key constraint"
let rollback_doc = "build.main_binary immediate foreign key constraint"

open Grej.Infix

let idx_build_job_start =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)"

let new_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       uuid VARCHAR(36) NOT NULL UNIQUE,
       start_d INTEGER NOT NULL,
       start_ps INTEGER NOT NULL,
       finish_d INTEGER NOT NULL,
       finish_ps INTEGER NOT NULL,
       result_kind TINYINT NOT NULL,
       result_code INTEGER,
       result_msg TEXT,
       console BLOB NOT NULL,
       script TEXT NOT NULL,
       main_binary INTEGER,
       user INTEGER NOT NULL,
       job INTEGER NOT NULL,

       FOREIGN KEY(main_binary) REFERENCES build_artifact(id) DEFERRABLE INITIALLY DEFERRED,
       FOREIGN KEY(user) REFERENCES user(id),
       FOREIGN KEY(job) REFERENCES job(id)
     )
  |}

let old_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       uuid VARCHAR(36) NOT NULL UNIQUE,
       start_d INTEGER NOT NULL,
       start_ps INTEGER NOT NULL,
       finish_d INTEGER NOT NULL,
       finish_ps INTEGER NOT NULL,
       result_kind TINYINT NOT NULL,
       result_code INTEGER,
       result_msg TEXT,
       console BLOB NOT NULL,
       script TEXT NOT NULL,
       main_binary INTEGER,
       user INTEGER NOT NULL,
       job INTEGER NOT NULL,

       FOREIGN KEY(main_binary) REFERENCES build_artifact(id),
       FOREIGN KEY(user) REFERENCES user(id),
       FOREIGN KEY(job) REFERENCES job(id)
     )
  |}

let copy_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "INSERT INTO new_build SELECT * from build"

let drop_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE build"

let rename_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "ALTER TABLE new_build RENAME TO build"

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec new_build () >>= fun () ->
  Db.exec copy_build () >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build () >>= fun () ->
  Db.exec copy_build () >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

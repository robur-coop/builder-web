let new_version = 7L and old_version = 6L
let identifier = "2021-06-09"
let migrate_doc = "add user column to build"
let rollback_doc = "remove user column from build"

let idx_build_job_start =
  Caqti_request.exec Caqti_type.unit
    "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)"

let nologin_user =
  Caqti_request.exec
    Caqti_type.unit
    "INSERT INTO user (username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p, restricted) \
     VALUES ('nologin', x'', x'', 16384, 8, 1, true)"

let remove_nologin_user =
  Caqti_request.exec
    Caqti_type.unit
    "DELETE FROM user WHERE username = 'nologin'"

let new_build =
  Caqti_request.exec
    Caqti_type.unit
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
         FOREIGN KEY(job) REFERENCES job(id),
         FOREIGN KEY(user) REFERENCES user(id)
       )
    |}

let old_build =
  Caqti_request.exec
    Caqti_type.unit
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
         job INTEGER NOT NULL,

         FOREIGN KEY(main_binary) REFERENCES build_artifact(id),
         FOREIGN KEY(job) REFERENCES job(id)
       )
    |}

let insert_from_old_build =
  Caqti_request.exec ~oneshot:true
    Builder_db.Rep.id
    {| INSERT INTO new_build (id, uuid, start_d, start_ps, finish_d, finish_ps,
                              result_kind, result_code, result_msg, console,
                              script, main_binary, job, user)
       SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind,
         result_code, result_msg, console, script, main_binary, job, ?
       FROM build |}

let insert_from_new_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    {| INSERT INTO new_build (id, uuid, start_d, start_ps, finish_d, finish_ps,
                              result_kind, result_code, result_msg, console,
                              script, main_binary, job)
       SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind,
         result_code, result_msg, console, script, main_binary, job
       FROM build |}

let drop_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "DROP TABLE build"

let rename_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "ALTER TABLE new_build RENAME TO build"

open Rresult.R.Infix

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec nologin_user () >>= fun () ->
  Db.find Builder_db.last_insert_rowid () >>= fun user_id ->
  Db.exec new_build () >>= fun () ->
  Db.exec insert_from_old_build user_id >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build () >>= fun () ->
  Db.exec insert_from_new_build () >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec remove_nologin_user () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()



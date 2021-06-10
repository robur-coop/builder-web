let old_version = 1L
let new_version = 2L
let identifier = "2021-02-16"
let migrate_doc = "change to scrypt hashed passwords (NB: destructive!!)"
let rollback_doc = "rollback scrypt hashed passwords (NB: destructive!!)"

let drop_user =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "DROP TABLE user"

let new_user =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    {| CREATE TABLE user (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         username VARCHAR(255) NOT NULL UNIQUE,
         password_hash BLOB NOT NULL,
         password_salt BLOB NOT NULL,
         scrypt_n INTEGER NOT NULL,
         scrypt_r INTEGER NOT NULL,
         scrypt_p INTEGER NOT NULL
       )
      |}

let old_user =
  Caqti_request.exec
    Caqti_type.unit
    {| CREATE TABLE user (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         username VARCHAR(255) NOT NULL UNIQUE,
         password_hash BLOB NOT NULL,
         password_salt BLOB NOT NULL,
         password_iter INTEGER NOT NULL
       )
    |}

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec drop_user () >>= fun () ->
  Db.exec new_user () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec drop_user () >>= fun () ->
  Db.exec old_user () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

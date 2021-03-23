let old_user_version = 1L
let new_user_version = 2L

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

let migrate (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:old_user_version (module Db) >>= fun () ->
  Db.exec drop_user () >>= fun () ->
  Db.exec new_user () >>= fun () ->
  Db.exec (Grej.set_version new_user_version) ()

let rollback (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:new_user_version (module Db) >>= fun () ->
  Db.exec drop_user () >>= fun () ->
  Db.exec old_user () >>= fun () ->
  Db.exec (Grej.set_version old_user_version) ()
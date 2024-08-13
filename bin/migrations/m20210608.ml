let new_version = 6L and old_version = 5L
let identifier = "2021-06-08"
let migrate_doc = "add access list"
let rollback_doc = "remove access list"

open Grej.Infix

let new_user =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_user (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       username VARCHAR(255) NOT NULL UNIQUE,
       password_hash BLOB NOT NULL,
       password_salt BLOB NOT NULL,
       scrypt_n INTEGER NOT NULL,
       scrypt_r INTEGER NOT NULL,
       scrypt_p INTEGER NOT NULL,
       restricted BOOLEAN NOT NULL
     )
  |}

let old_user =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_user (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       username VARCHAR(255) NOT NULL UNIQUE,
       password_hash BLOB NOT NULL,
       password_salt BLOB NOT NULL,
       scrypt_n INTEGER NOT NULL,
       scrypt_r INTEGER NOT NULL,
       scrypt_p INTEGER NOT NULL
     )
  |}

let collect_old_user =
  Caqti_type.unit ->*
  Caqti_type.(t4 int64 string (t2 octets octets) (t3 int64 int64 int64)) @@
  "SELECT id, username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p FROM user"

let collect_new_user =
  Caqti_type.unit ->*
  Caqti_type.(t4 int64 string (t2 octets octets) (t4 int64 int64 int64 bool)) @@
  "SELECT id, username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p, restricted FROM user"

let insert_new_user =
  Caqti_type.(t4 int64 string (t2 octets octets) (t4 int64 int64 int64 bool)) ->.
  Caqti_type.unit @@
  "INSERT INTO new_user (id, username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p, restricted) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

let insert_old_user =
  Caqti_type.(t4 int64 string (t2 octets octets) (t3 int64 int64 int64)) ->.
  Caqti_type.unit @@
  "INSERT INTO new_user (id, username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p) VALUES (?, ?, ?, ?, ?, ?, ?)"

let drop_user =
  Caqti_type.unit ->. Caqti_type.unit @@
    "DROP TABLE user"

let rename_new_user =
  Caqti_type.unit ->. Caqti_type.unit @@
    "ALTER TABLE new_user RENAME TO user"

let access_list =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE access_list (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         user INTEGER NOT NULL,
         job INTEGER NOT NULL,

         FOREIGN KEY(user) REFERENCES user(id),
         FOREIGN KEY(job) REFERENCES job(id),
         UNIQUE(user, job)
       )
  |}

let rollback_access_list =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE IF EXISTS access_list"

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec new_user () >>= fun () ->
  Db.collect_list collect_old_user () >>= fun users ->
  Grej.list_iter_result (fun (id, username, (password_hash, password_salt), (scrypt_n, scrypt_r, scrypt_p)) ->
      Db.exec insert_new_user (id, username, (password_hash, password_salt), (scrypt_n, scrypt_r, scrypt_p, false)))
    users >>= fun () ->
  Db.exec drop_user () >>= fun () ->
  Db.exec rename_new_user () >>= fun () ->
  Db.exec access_list () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_user () >>= fun () ->
  Db.collect_list collect_new_user () >>= fun users ->
  Grej.list_iter_result (fun (id, username, (password_hash, password_salt), (scrypt_n, scrypt_r, scrypt_p, restricted)) ->
      if restricted then Logs.warn (fun m -> m "elevating privileges of restricted user %s" username);
      Db.exec insert_old_user (id, username, (password_hash, password_salt), (scrypt_n, scrypt_r, scrypt_p)))
    users >>= fun () ->
  Db.exec drop_user () >>= fun () ->
  Db.exec rename_new_user () >>= fun () ->
  Db.exec rollback_access_list () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

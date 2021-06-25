let new_version = 8L and old_version = 7L
let identifier = "2021-06-25"
let migrate_doc = "drop build_file table"
let rollback_doc = "recreate build_file table"

let build_file =
  Caqti_request.exec
    Caqti_type.unit
    {| CREATE TABLE build_file (
           id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
           filepath TEXT NOT NULL, -- the path as in the build
           localpath TEXT NOT NULL, -- local path to the file on disk
           sha256 BLOB NOT NULL,
           size INTEGER NOT NULL,
           build INTEGER NOT NULL,

           FOREIGN KEY(build) REFERENCES build(id),
           UNIQUE(build, filepath)
         )
      |}

let drop_build_file =
  Caqti_request.exec
    Caqti_type.unit
    "DROP TABLE build_file"

open Rresult.R.Infix

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec drop_build_file () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec build_file () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

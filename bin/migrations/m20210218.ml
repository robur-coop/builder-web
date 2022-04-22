let old_version = 2L
let new_version = 3L
let identifier = "2021-02-18"
let migrate_doc = "add column size to build_file and build_artifact"
let rollback_doc = "remove column size to build_file and build_artifact"

open Grej.Infix

let new_build_artifact =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build_artifact (
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

let new_build_file =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build_file (
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

let collect_build_artifact =
  Caqti_type.unit ->* Caqti_type.(tup3 int64 (tup3 string string octets) int64) @@
  "SELECT id, filepath, localpath, sha256, build FROM build_artifact"

let collect_build_file =
  Caqti_type.unit ->* Caqti_type.(tup3 int64 (tup3 string string octets) int64) @@
  "SELECT id, filepath, localpath, sha256, build FROM build_file"

let insert_new_build_artifact =
  Caqti_type.(tup3 int64 (tup4 string string octets int64) int64) ->. Caqti_type.unit @@
  {| INSERT INTO new_build_artifact (id, filepath, localpath, sha256, size, build)
     VALUES (?, ?, ?, ?, ?, ?)
  |}

let insert_new_build_file =
  Caqti_type.(tup3 int64 (tup4 string string octets int64) int64) ->. Caqti_type.unit @@
  {| INSERT INTO new_build_file (id, filepath, localpath, sha256, size, build)
     VALUES (?, ?, ?, ?, ?, ?)
  |}

let drop_build_artifact =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE build_artifact"

let drop_build_file =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE build_file"

let rename_build_artifact =
  Caqti_type.unit ->. Caqti_type.unit @@
  "ALTER TABLE new_build_artifact RENAME TO build_artifact"

let rename_build_file =
  Caqti_type.unit ->. Caqti_type.unit @@
  "ALTER TABLE new_build_file RENAME TO build_file"

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec new_build_artifact () >>= fun () ->
  Db.rev_collect_list collect_build_artifact () >>= fun build_artifacts ->
  Grej.list_iter_result
    (fun (id, (filepath, localpath, sha256), build) ->
       let stats = Unix.stat localpath in
       Db.exec insert_new_build_artifact
         (id, (filepath, localpath, sha256, Int64.of_int stats.st_size), build))
    build_artifacts >>= fun () ->
  Db.exec drop_build_artifact () >>= fun () ->
  Db.exec rename_build_artifact () >>= fun () ->

  Db.exec new_build_file () >>= fun () ->
  Db.rev_collect_list collect_build_file () >>= fun build_files ->
  Grej.list_iter_result
    (fun (id, (filepath, localpath, sha256), build) ->
       let stats = Unix.stat localpath in
       Db.exec insert_new_build_file
         (id, (filepath, localpath, sha256, Int64.of_int stats.st_size), build))
    build_files >>= fun () ->
  Db.exec drop_build_file () >>= fun () ->
  Db.exec rename_build_file () >>= fun () ->

  Db.exec (Grej.set_version new_version) ()

let old_build_artifact =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build_artifact (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         filepath TEXT NOT NULL, -- the path as in the build
         localpath TEXT NOT NULL, -- local path to the file on disk
         sha256 BLOB NOT NULL,
         build INTEGER NOT NULL,

         FOREIGN KEY(build) REFERENCES build(id),
         UNIQUE(build, filepath)
       )
  |}

let old_build_file =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build_file (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         filepath TEXT NOT NULL, -- the path as in the build
         localpath TEXT NOT NULL, -- local path to the file on disk
         sha256 BLOB NOT NULL,
         build INTEGER NOT NULL,

         FOREIGN KEY(build) REFERENCES build(id),
         UNIQUE(build, filepath)
       )
  |}

let copy_build_artifact =
  Caqti_type.unit ->. Caqti_type.unit @@
  "INSERT INTO new_build_artifact SELECT id, filepath, localpath, sha256, build FROM build_artifact"

let copy_build_file =
  Caqti_type.unit ->. Caqti_type.unit @@
  "INSERT INTO new_build_file SELECT id, filepath, localpath, sha256, build FROM build_file"

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build_artifact () >>= fun () ->
  Db.exec copy_build_artifact () >>= fun () ->
  Db.exec drop_build_artifact () >>= fun () ->
  Db.exec rename_build_artifact () >>= fun () ->

  Db.exec old_build_file () >>= fun () ->
  Db.exec copy_build_file () >>= fun () ->
  Db.exec drop_build_file () >>= fun () ->
  Db.exec rename_build_file () >>= fun () ->

  Db.exec (Grej.set_version old_version) ()

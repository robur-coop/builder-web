let new_version = 18L and old_version = 17L
and identifier = "2023-09-14"
and migrate_doc = "Artifacts are stored content-addressed in the filesystem"
and rollback_doc = "Artifacts are stored under their build's job name and uuid"

open Grej.Syntax

let new_build_artifact =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build_artifact (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         filepath TEXT NOT NULL,
         sha256 BLOB NOT NULL,
         size INTEGER NOT NULL,
         build INTEGER NOT NULL,

         FOREIGN KEY(build) REFERENCES build(id),
         UNIQUE(build, filepath)
       )
  |}

let old_build_artifact =
  Caqti_type.unit ->.  Caqti_type.unit @@
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

let idx_build_artifact_sha256 =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_artifact_sha256 ON build_artifact(sha256)"

let idx_build_artifact_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_artifact_build ON build_artifact(build)"

let copy_new_build_artifact =
  Caqti_type.(unit ->. unit) @@
  {| INSERT INTO new_build_artifact(id, filepath, sha256, size, build)
     SELECT id, filepath, sha256, size, build
     FROM build_artifact
  |}

let copy_old_build_artifact =
  Caqti_type.(unit ->. unit) @@
  {| INSERT INTO new_build_artifact(id, filepath, localpath, sha256, size, build)
     SELECT a.id, a.filepath,
       j.name || '/' || b.uuid || '/output/' || a.filepath,
       a.sha256, a.size, a.build
     FROM build_artifact a, job j, build b
     WHERE b.id = a.build AND j.id = b.job
  |}

let new_build_artifact_paths =
  Caqti_type.unit ->* Caqti_type.(t2 string string) @@
  {| SELECT localpath, '_artifacts/' || substr(lower(hex(sha256)), 1, 2) || '/' || lower(hex(sha256))
     FROM build_artifact
  |}

let old_build_artifact_paths =
  Caqti_type.unit ->* Caqti_type.(t2 string string) @@
  {| SELECT '_artifacts/' || substr(lower(hex(a.sha256)), 1, 2) || '/' || lower(hex(a.sha256)),
       j.name || '/' || b.uuid || '/output/' || a.filepath
     FROM build_artifact a, job j, build b
     WHERE b.id = a.build AND j.id = b.job
  |}

let drop_build_artifact =
  Caqti_type.(unit ->. unit) @@
  "DROP TABLE build_artifact"

let rename_build_artifact =
  Caqti_type.(unit ->. unit) @@
  "ALTER TABLE new_build_artifact RENAME TO build_artifact"

let move_paths ?force datadir (old_path, new_path) =
  let old_path = Fpath.(datadir // v old_path) and new_path = Fpath.(datadir // v new_path) in
  let* _created = Bos.OS.Dir.create (Fpath.parent new_path) in
  Bos.OS.Path.move ?force old_path new_path

let copy_paths datadir (old_path, new_path) =
  let old_path = Fpath.(datadir // v old_path) and new_path = Fpath.(datadir // v new_path) in
  let new_path_tmp = Fpath.(new_path + "tmp") in
  let* _created = Bos.OS.Dir.create (Fpath.parent new_path) in
  let cmd = Bos.Cmd.(v "cp" % p old_path % p new_path_tmp) in
  let* () =
    match Bos.OS.Cmd.run_status cmd with
    | Ok `Exited 0 ->
      Ok ()
    | Ok status ->
      let _ = Bos.OS.Path.delete new_path_tmp in
      Error (`Msg (Fmt.str "cp failed: %a" Bos.OS.Cmd.pp_status status))
    | Error _ as e ->
      let _ = Bos.OS.Path.delete new_path_tmp in
      e
  in
  Bos.OS.Path.move ~force:true new_path_tmp new_path

let migrate datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:old_version (module Db) in
  let* () = Db.exec new_build_artifact () in
  let* () = Db.exec copy_new_build_artifact () in
  let* () = Db.iter_s new_build_artifact_paths (move_paths ~force:true datadir) () in
  let* () = Db.exec drop_build_artifact () in
  let* () = Db.exec rename_build_artifact () in
  let* () = Db.exec idx_build_artifact_sha256 () in
  let* () = Db.exec idx_build_artifact_build () in
  Db.exec (Grej.set_version new_version) ()

let rollback datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:new_version (module Db) in
  let* () = Db.exec old_build_artifact () in
  let* () = Db.exec copy_old_build_artifact () in
  let* () = Db.iter_s old_build_artifact_paths (copy_paths datadir) () in
  let* () =
    Db.iter_s old_build_artifact_paths
      (fun (old_path, _new_path) ->
         Bos.OS.Path.delete Fpath.(datadir // v old_path))
      ()
  in
  let* () = Db.exec drop_build_artifact () in
  let* () = Db.exec rename_build_artifact () in
  let* () = Db.exec idx_build_artifact_sha256 () in
  Db.exec (Grej.set_version old_version) ()

(* migration failed but managed to move *some* files *)
let fixup_migrate datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:old_version (module Db) in
  let* () =
    Db.iter_s new_build_artifact_paths
      (fun (old_path, new_path) ->
         let* old_exists = Bos.OS.Path.exists Fpath.(datadir // v old_path) in
         let* new_exists = Bos.OS.Path.exists Fpath.(datadir // v new_path) in
         if new_exists && not old_exists then
           copy_paths datadir (new_path, old_path)
         else Ok ())
      ()
  in
  Db.iter_s new_build_artifact_paths
    (fun (_old_path, new_path) ->
       Bos.OS.Path.delete Fpath.(datadir // v new_path))
    ()

(* rollback failed but some or all artifacts were copied *)
let fixup_rollback datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:new_version (module Db) in
  Db.iter_s old_build_artifact_paths
    (fun (old_path, new_path) ->
       let* old_exists = Bos.OS.Path.exists Fpath.(datadir // v old_path) in
       if old_exists then
         Bos.OS.Path.delete Fpath.(datadir // v new_path)
       else
         move_paths datadir (new_path, old_path))
    ()

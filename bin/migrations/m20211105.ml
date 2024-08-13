let new_version = 15L and old_version = 14L
and identifier = "2021-11-05"
and migrate_doc = "add platform to build"
and rollback_doc = "remove platform from build"

open Grej.Syntax

let new_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       uuid VARCHAR(36) NOT NULL UNIQUE,
       start_d INTEGER NOT NULL,
       start_ps INTEGER NOT NULL,
       finish_d INTEGER NOT NULL,
       finish_ps INTEGER NOT NULL,
       result_code INTEGER NOT NULL,
       result_msg TEXT,
       console TEXT NOT NULL,
       script TEXT NOT NULL,
       platform TEXT NOT NULL,
       main_binary INTEGER,
       user INTEGER NOT NULL,
       job INTEGER NOT NULL,
       input_id BLOB, -- sha256 (sha256<opam-switch> || sha256<build-environment> || sha256<system-packages>)

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
       result_code INTEGER NOT NULL,
       result_msg TEXT,
       console TEXT NOT NULL,
       script TEXT NOT NULL,
       main_binary INTEGER,
       user INTEGER NOT NULL,
       job INTEGER NOT NULL,
       input_id BLOB, -- sha256 (sha256<opam-switch> || sha256<build-environment> || sha256<system-packages>)

       FOREIGN KEY(main_binary) REFERENCES build_artifact(id) DEFERRABLE INITIALLY DEFERRED,
       FOREIGN KEY(user) REFERENCES user(id),
       FOREIGN KEY(job) REFERENCES job(id)
     )
  |}

let copy_from_old_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| INSERT INTO new_build(id, uuid, start_d, start_ps, finish_d, finish_ps,
       result_code, result_msg, console, script, platform, main_binary, user, job, input_id)
     SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_code, result_msg,
       console, script, 'PLACEHOLDER-PLATFORM', main_binary, user, job, input_id
     FROM build
  |}

let copy_from_new_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| INSERT INTO new_build(id, uuid, start_d, start_ps, finish_d, finish_ps,
       result_code, result_msg, console, script, main_binary, user, job, input_id)
     SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_code, result_msg,
       console, script, main_binary, user, job, input_id
     FROM build
  |}

let build_id_and_user =
  Caqti_type.unit ->* Caqti_type.(t2 (Builder_db.Rep.id (`build : [ `build ])) int64) @@
  "SELECT id, user FROM build"

let update_new_build_platform =
  Caqti_type.(t2 (Builder_db.Rep.id (`build : [ `build ])) string) ->. Caqti_type.unit @@
  "UPDATE new_build SET platform = $2 WHERE id = $1"

let drop_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE build"

let rename_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "ALTER TABLE new_build RENAME TO build"

(*
1|reynir
2|freebsd-builder
3|ubuntu-builder
5|nologin
6|reynir-solsort
7|reynir-spurv
*)
let platform_of_user_id = function
  | 1L -> assert false
  | 2L -> "freebsd-12"
  | 3L -> "ubuntu-20.04"
  | 5L -> assert false
  | 6L -> "debian-10"
  | 7L -> "debian-11"
  | _ -> assert false

let idx_build_job_start =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)"

let idx_build_failed =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_failed ON build(job, start_d DESC, start_ps DESC) WHERE result_code <> 0"

let idx_build_input_id =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_input_id ON build(input_id)"

let idx_build_main_binary =
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_main_binary ON build(main_binary)"

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:old_version (module Db) in
  let* () = Db.exec new_build () in
  let* () = Db.exec copy_from_old_build () in
  let* builds = Db.collect_list build_id_and_user () in
  let* () =
    Grej.list_iter_result (fun (id, user) ->
        let platform = platform_of_user_id user in
        Db.exec update_new_build_platform (id, platform))
      builds
  in
  let* () = Db.exec drop_build () in
  let* () = Db.exec rename_build () in
  let* () = Db.exec idx_build_job_start () in
  let* () = Db.exec idx_build_failed () in
  let* () = Db.exec idx_build_input_id () in
  let* () = Db.exec idx_build_main_binary () in
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  let* () = Grej.check_version ~user_version:new_version (module Db) in
  let* () = Db.exec old_build () in
  let* () = Db.exec copy_from_new_build () in
  let* () = Db.exec drop_build () in
  let* () = Db.exec rename_build () in
  let* () = Db.exec idx_build_job_start () in
  let* () = Db.exec idx_build_failed () in
  let* () = Db.exec idx_build_input_id () in
  let* () = Db.exec idx_build_main_binary () in
  Db.exec (Grej.set_version old_version) ()


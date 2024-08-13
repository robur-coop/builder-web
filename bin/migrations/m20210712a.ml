let new_version = 13L and old_version = 12L
and identifier = "2021-07-12a"
and migrate_doc = "remove result_kind from build, add indexes idx_build_failed and idx_build_artifact_sha256"
and rollback_doc = "add result_kind to build, remove indexes idx_build_failed and idx_build_artifact_sha256"

open Grej.Infix

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
       console BLOB NOT NULL,
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

let copy_old_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| INSERT INTO new_build(id, uuid, start_d, start_ps, finish_d, finish_ps,
       result_code, result_msg, console, script, main_binary, user, job, input_id)
     SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, 0, result_msg,
       console, script, main_binary, user, job, input_id
     FROM build
  |}

let old_build_execution_result =
  Caqti_type.unit ->*
  Caqti_type.(t3 (Builder_db.Rep.id (`build : [ `build ])) int (option int)) @@
  "SELECT id, result_kind, result_code FROM build"

let update_new_build_execution_result =
  Caqti_type.(t2 (Builder_db.Rep.id (`build : [ `build ])) int) ->. Caqti_type.unit @@
  "UPDATE new_build SET result_code = $2 WHERE id = $1"

let old_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE new_build (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       uuid VARCHAR(36) NOT NULL UNIQUE,
       start_d INTEGER NOT NULL,
       start_ps INTEGER NOT NULL,
       finish_d INTEGER NOT NULL,
       finish_ps INTEGER NOT NULL,
       result_kind INTEGER NOT NULL,
       result_code INTEGER,
       result_msg TEXT,
       console BLOB NOT NULL,
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

let copy_new_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| INSERT INTO new_build(id, uuid, start_d, start_ps, finish_d, finish_ps,
       result_kind, result_msg, console, script, main_binary, user, job, input_id)
     SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, 0, result_msg,
       console, script, main_binary, user, job, input_id
     FROM build
  |}

let new_build_execution_result =
  Caqti_type.unit ->*
  Caqti_type.(t2 (Builder_db.Rep.id (`build : [ `build ])) int) @@
  "SELECT id, result_code FROM build"

let update_old_build_execution_result =
  Caqti_type.(t3 (Builder_db.Rep.id (`build : [ `build ])) int (option int)) ->.
  Caqti_type.unit @@
  "UPDATE new_build SET result_kind = $2, result_code = $3 WHERE id = $1"

let drop_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE build"

let rename_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "ALTER TABLE new_build RENAME TO build"

let execution_new_of_old kind code =
  match kind, code with
  | 0, Some v -> Ok v
  | 1, Some v -> Ok (v lsl 8)
  | 2, Some v -> Ok (v lsl 16)
  | 3, None -> Ok 65536
  | _ -> Error (`Msg "bad encoding")

let execution_old_of_new code =
  if code <= 0xFF
  then Ok (0, Some code)
  else if code <= 0xFFFF
  then Ok (1, Some (code lsr 8))
  else if code <= 0xFFFFFF
  then Ok (2, Some (code lsr 16))
  else if code = 65536
  then Ok (3, None)
  else Error (`Msg "bad encoding")

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec new_build () >>= fun () ->
  Db.exec copy_old_build () >>= fun () ->
  Db.collect_list old_build_execution_result () >>= fun results ->
  Grej.list_iter_result (fun (id, kind, code) ->
    execution_new_of_old kind code >>= fun code' ->
    Db.exec update_new_build_execution_result (id, code'))
    results >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)")
    () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "CREATE INDEX idx_build_failed ON build(job, start_d DESC, start_ps DESC) \
            WHERE result_code <> 0")
    () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "CREATE INDEX idx_build_input_id ON build(input_id)")
    () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "CREATE INDEX idx_build_main_binary ON build(main_binary)")
    () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "CREATE INDEX idx_build_artifact_sha256 ON build_artifact(sha256)")
    () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build () >>= fun () ->
  Db.exec copy_new_build () >>= fun () ->
  Db.collect_list new_build_execution_result () >>= fun results ->
  Grej.list_iter_result (fun (id, code) ->
    execution_old_of_new code >>= fun (kind, code') ->
    Db.exec update_old_build_execution_result (id, kind, code'))
    results >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "DROP INDEX idx_build_artifact_sha256") () >>= fun () ->
  Db.exec (Caqti_type.unit ->. Caqti_type.unit @@
           "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)")
    () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

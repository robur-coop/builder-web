let new_version = 12L and old_version = 11L
let identifier = "2021-07-06"
let migrate_doc = "add a input_id column to the build table"
let rollback_doc = "remove the input_id column from the build table"

let add_input_id_to_build =
    Caqti_request.exec
      Caqti_type.unit
      {| ALTER TABLE build ADD COLUMN input_id BLOB |}

let idx_build_job_start =
  Caqti_request.exec Caqti_type.unit
    "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)"

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
           user INTEGER NOT NULL,
           job INTEGER NOT NULL,

           FOREIGN KEY(main_binary) REFERENCES build_artifact(id) DEFERRABLE INITIALLY DEFERRED,
           FOREIGN KEY(user) REFERENCES user(id),
           FOREIGN KEY(job) REFERENCES job(id)
         )
      |}

let copy_build =
  Caqti_request.exec
    Caqti_type.unit
    "INSERT INTO new_build SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind, result_code, result_msg, console, script, main_binary, user, job FROM build"

let drop_build =
  Caqti_request.exec
    Caqti_type.unit
    "DROP TABLE build"

let rename_build =
  Caqti_request.exec
    Caqti_type.unit
    "ALTER TABLE new_build RENAME TO build"

let drop_input_id_from_build =
    Caqti_request.exec
      Caqti_type.unit
      {| ALTER TABLE build DROP COLUMN input_id |}

let builds =
  Caqti_request.collect
    Caqti_type.unit
    (Caqti_type.tup4
       Builder_db.Rep.untyped_id
       Builder_db.Rep.cstruct
       Builder_db.Rep.cstruct
       Builder_db.Rep.cstruct)
    {| SELECT b.id, opam.sha256, env.sha256, system.sha256
       FROM build b, build_artifact opam, build_artifact env, build_artifact system
       WHERE opam.filepath = 'opam-switch' AND env.filepath = 'build-environment'
         AND system.filepath = 'system-packages'
         AND opam.build = b.id AND env.build = b.id AND system.build = b.id
    |}

let set_input_id =
  Caqti_request.exec
    (Caqti_type.tup2 Builder_db.Rep.untyped_id Builder_db.Rep.cstruct)
    "UPDATE build SET input_id = ?2 WHERE id = ?1"

open Rresult.R.Infix

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec add_input_id_to_build () >>= fun () ->
  Db.collect_list builds () >>= fun builds ->
  Grej.list_iter_result (fun (id, opam_sha, env_sha, pkg_sha) ->
     let input_id = Mirage_crypto.Hash.SHA256.digest (Cstruct.concat [ opam_sha ; env_sha ; pkg_sha ]) in
     Db.exec set_input_id (id, input_id))
    builds >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build () >>= fun () ->
  Db.exec copy_build () >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

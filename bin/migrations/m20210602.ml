let old_version = 4L and new_version = 5L
let identifier = "2021-06-02"
let migrate_doc = "build.main_binary foreign key"
let rollback_doc = "build.main_binary filepath"

let idx_build_job_start =
  Caqti_request.exec Caqti_type.unit
    "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)"

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
           job INTEGER NOT NULL,

           FOREIGN KEY(main_binary) REFERENCES build_artifact(id),
           FOREIGN KEY(job) REFERENCES job(id)
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
           main_binary TEXT,
           job INTEGER NOT NULL,

           FOREIGN KEY(job) REFERENCES job(id)
         )
      |}

let collect_old_build =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    Caqti_type.(tup3 Builder_db.Rep.id
      (tup3 (tup4 string int64 int64 int64) (tup4 int64 int (option int) (option string)) (tup3 octets string (option string)))
      Builder_db.Rep.id)
    {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind, result_code, result_msg,
              console, script, main_binary, job
       FROM build |}

let insert_new_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.(tup3 Builder_db.Rep.id
      (tup3 (tup4 string int64 int64 int64) (tup4 int64 int (option int) (option string)) (tup3 octets string (option Builder_db.Rep.id)))
      Builder_db.Rep.id)
    {| INSERT INTO new_build (id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind,
                              result_code, result_msg, console, script, main_binary, job)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |}

let drop_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "DROP TABLE build"

let rename_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "ALTER TABLE new_build RENAME TO build"

let find_main_artifact_id =
  Caqti_request.find ~oneshot:true
    Caqti_type.(tup2 Builder_db.Rep.id string)
    Builder_db.Rep.id
    "SELECT id FROM build_artifact WHERE build = ?1 AND filepath = ?2"

let find_main_artifact_filepath =
  Caqti_request.find ~oneshot:true
    Builder_db.Rep.id
    Caqti_type.string
    "SELECT filepath FROM build_artifact WHERE id = ?"

let collect_new_build =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    Caqti_type.(tup3 Builder_db.Rep.id
      (tup3 (tup4 string int64 int64 int64) (tup4 int64 int (option int) (option string)) (tup3 octets string (option Builder_db.Rep.id)))
      Builder_db.Rep.id)
    {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind, result_code, result_msg,
              console, script, main_binary, job
       FROM build |}
  
let insert_old_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.(tup3 Builder_db.Rep.id
      (tup3 (tup4 string int64 int64 int64) (tup4 int64 int (option int) (option string)) (tup3 octets string (option string)))
      Builder_db.Rep.id)
    {| INSERT INTO new_build (id, uuid, start_d, start_ps, finish_d, finish_ps, result_kind,
                              result_code, result_msg, console, script, main_binary, job)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |}

let migrate _ (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec new_build () >>= fun () ->
  Db.rev_collect_list collect_old_build () >>= fun builds ->
  Grej.list_iter_result
    (fun (id, ((uuid, start_d, start_ps, finish_d), (finish_ps, result_kind, result_code, result_msg), (console, script, main_binary)), job) ->
       (match main_binary with
         | None -> Ok None
         | Some path -> Db.find find_main_artifact_id (id, path) >>| fun id -> Some id)
       >>= fun main_binary_id ->
       Db.exec insert_new_build
         (id, ((uuid, start_d, start_ps, finish_d), (finish_ps, result_kind, result_code, result_msg), (console, script, main_binary_id)), job)) 
    builds >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

 
let rollback _ (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build () >>= fun () ->
  Db.rev_collect_list collect_new_build () >>= fun builds ->
  Grej.list_iter_result
    (fun (id, ((uuid, start_d, start_ps, finish_d), (finish_ps, result_kind, result_code, result_msg), (console, script, main_binary)), job) ->
       (match main_binary with
         | None -> Ok None
         | Some main_binary_id -> Db.find find_main_artifact_filepath main_binary_id >>| fun filepath -> Some filepath)
       >>= fun filepath ->
       Db.exec insert_old_build
         (id, ((uuid, start_d, start_ps, finish_d), (finish_ps, result_kind, result_code, result_msg), (console, script, filepath)), job)) 
    builds >>= fun () ->
  Db.exec drop_build () >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec idx_build_job_start () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

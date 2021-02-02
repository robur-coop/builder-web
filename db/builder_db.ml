module Rep = Representation
open Rep

let application_id = 1234839235l

(* Please update this when making changes! *)
let current_version = 1L

type id = Rep.id

type file = Rep.file = {
  filepath : Fpath.t;
  localpath : Fpath.t;
  sha256 : Cstruct.t;
}

let last_insert_rowid =
  Caqti_request.find
    Caqti_type.unit
    id
    "SELECT last_insert_rowid()"


let get_application_id =
  Caqti_request.find
    Caqti_type.unit
    Caqti_type.int32
    "PRAGMA application_id"

let get_version =
  Caqti_request.find
    Caqti_type.unit
    Caqti_type.int64
    "PRAGMA user_version"

let set_application_id =
  Caqti_request.exec
    Caqti_type.unit
    (Printf.sprintf "PRAGMA application_id = %ld" application_id)

let set_current_version =
  Caqti_request.exec
    Caqti_type.unit
    (Printf.sprintf "PRAGMA user_version = %Ld" current_version)

module Job = struct
  let migrate =
    Caqti_request.exec
      Caqti_type.unit
      {| CREATE TABLE job (
           id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
           name VARCHAR(255) NOT NULL UNIQUE
         )
      |}

  let rollback =
    Caqti_request.exec
      Caqti_type.unit
      {| DROP TABLE IF EXISTS job |}

  let get =
    Caqti_request.find_opt
      id
      Caqti_type.string
      "SELECT name FROM job WHERE id = ?"

  let get_id_by_name =
    Caqti_request.find
      Caqti_type.string
      id
      "SELECT id FROM job WHERE name = ?"

  let get_all =
    Caqti_request.collect
      Caqti_type.unit
      Caqti_type.(tup2 id string)
      "SELECT id, name FROM job ORDER BY name ASC"

  let try_add =
    Caqti_request.exec
      Caqti_type.string
      "INSERT OR IGNORE INTO job (name) VALUES (?)"

  let remove =
    Caqti_request.exec
      id
      "DELETE FROM job WHERE id = ?"
end

module Build_artifact = struct
  let migrate =
    Caqti_request.exec
      Caqti_type.unit
      {| CREATE TABLE build_artifact (
             id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
             filepath TEXT NOT NULL, -- the path as in the build
             localpath TEXT NOT NULL, -- local path to the file on disk
             sha256 BLOB NOT NULL,
             build INTEGER NOT NULL,

             FOREIGN KEY(build) REFERENCES build(id),
             UNIQUE(build, filepath)
           )
        |}

  let rollback =
    Caqti_request.exec
      Caqti_type.unit
      "DROP TABLE IF EXISTS build_artifact"

  let get_by_build =
    Caqti_request.find
      (Caqti_type.tup2 id fpath)
      (Caqti_type.tup2 id file)
      {| SELECT id, filepath, localpath, sha256
         FROM build_artifact
         WHERE build = ? AND filepath = ?
      |}

  let get_by_build_uuid =
    Caqti_request.find_opt
      (Caqti_type.tup2 uuid fpath)
      (Caqti_type.tup2 id file)
      {| SELECT build_artifact.id, build_artifact.filepath,
           build_artifact.localpath, build_artifact.sha256
         FROM build_artifact
         INNER JOIN build ON build.id = build_artifact.build
         WHERE build.uuid = ? AND build_artifact.filepath = ?
      |}

  let get_all_by_build =
    Caqti_request.collect
      id
      Caqti_type.(tup2
                    id
                    file)
      "SELECT id, filepath, localpath, sha256 FROM build_artifact WHERE build = ?"

  let add =
    Caqti_request.exec
      Caqti_type.(tup2 file id)
      "INSERT INTO build_artifact (filepath, localpath, sha256, build)
        VALUES (?, ?, ?, ?)"

  let remove_by_build =
    Caqti_request.exec
      id
      "DELETE FROM build_artifact WHERE build = ?"
end

module Build_file = struct
  let migrate =
    Caqti_request.exec
      Caqti_type.unit
      {| CREATE TABLE build_file (
             id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
             filepath TEXT NOT NULL, -- the path as in the build
             localpath TEXT NOT NULL, -- local path to the file on disk
             sha256 BLOB NOT NULL,
             build INTEGER NOT NULL,

             FOREIGN KEY(build) REFERENCES build(id),
             UNIQUE(build, filepath)
           )
        |}

  let rollback =
    Caqti_request.exec
      Caqti_type.unit
      "DROP TABLE IF EXISTS build_file"

  let get_by_build_uuid =
    Caqti_request.find_opt
      (Caqti_type.tup2 uuid fpath)
      (Caqti_type.tup2 fpath cstruct)
      {| SELECT build_file.localpath, build_file.sha256
         FROM build_file
         INNER JOIN build ON build.id = build_file.build
         WHERE build.uuid = ? AND build_file.filepath = ?
      |}

  let get_all_by_build =
    Caqti_request.collect
      id
      Caqti_type.(tup2
                    id
                    file)
      "SELECT id, filepath, localpath, sha256 FROM build_file WHERE build = ?"

  let add =
    Caqti_request.exec
      Caqti_type.(tup2 file id)
      "INSERT INTO build_file (filepath, localpath, sha256, build)
        VALUES (?, ?, ?, ?)"

  let remove_by_build =
    Caqti_request.exec
      id
      "DELETE FROM build_file WHERE build = ?"
end

module Build = struct
  type t = {
    uuid : Uuidm.t;
    start : Ptime.t;
    finish : Ptime.t;
    result : Builder.execution_result;
    console : (int * string) list;
    script : string;
    main_binary : Fpath.t option;
    job_id : id;
  }

  let t =
    let rep =
      Caqti_type.(tup2
                    (tup4
                       uuid
                       (tup2
                          Rep.ptime
                          Rep.ptime)
                       (tup2
                          execution_result
                          console)
                       (tup2
                          string
                          (option Rep.fpath)))
                    id)
    in
    let encode { uuid; start; finish; result; console; script; main_binary; job_id } =
      Ok ((uuid, (start, finish), (result, console), (script, main_binary)), job_id)
    in
    let decode ((uuid, (start, finish), (result, console), (script, main_binary)), job_id) =
      Ok { uuid; start; finish; result; console; script; main_binary; job_id }
    in
    Caqti_type.custom ~encode ~decode rep

  module Meta = struct
    type t = {
      uuid : Uuidm.t;
      start : Ptime.t;
      finish : Ptime.t;
      result : Builder.execution_result;
      main_binary : Fpath.t option;
      job_id : id;
    }

    let t =
      let rep =
        Caqti_type.(tup2
                     (tup4
                       uuid
                       (tup2
                          Rep.ptime
                          Rep.ptime)
                       execution_result
                       (option Rep.fpath))
                     id)
      in
      let encode { uuid; start; finish; result; main_binary; job_id } =
        Ok ((uuid, (start, finish), result, main_binary), job_id)
      in
      let decode ((uuid, (start, finish), result, main_binary), job_id) =
        Ok { uuid; start; finish; result; main_binary; job_id }
      in
      Caqti_type.custom ~encode ~decode rep
  end

  let migrate =
    Caqti_request.exec
      Caqti_type.unit
      {| CREATE TABLE build (
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

  let rollback =
    Caqti_request.exec
      Caqti_type.unit
      {| DROP TABLE IF EXISTS build |}

  let get_opt =
    Caqti_request.find_opt
      Caqti_type.int64
      t
      {| SELECT uuid, start_d, start_ps, finish_d, finish_ps,
                result_kind, result_code, result_msg,
                console, script, main_binary, job
           FROM build
           WHERE id = ?
        |}

  let get_by_uuid =
    Caqti_request.find_opt
      Rep.uuid
      (Caqti_type.tup2 id t)
      {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                  result_kind, result_code, result_msg,
                  console, script, main_binary, job
           FROM build
           WHERE uuid = ?
        |}

  let get_all =
    Caqti_request.collect
      Caqti_type.int64
      (Caqti_type.tup2 id t)
      {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                  result_kind, result_code, result_msg, console,
                  script, main_binary, job
           FROM build
           WHERE job = ?
           ORDER BY start_d DESC, start_ps DESC
        |}

  let get_all_meta =
    Caqti_request.collect
      Caqti_type.int64
      (Caqti_type.tup2
         id Meta.t)
      {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                  result_kind, result_code, result_msg, main_binary, job
           FROM build
           WHERE job = ?
           ORDER BY start_d DESC, start_ps DESC
        |}

  let get_all_meta_by_name =
    Caqti_request.collect
      Caqti_type.string
      (Caqti_type.tup3
         id
         Meta.t
         file_opt)
      {| SELECT build.id, build.uuid,
                build.start_d, build.start_ps, build.finish_d, build.finish_ps,
                build.result_kind, build.result_code, build.result_msg,
                build.main_binary, build.job,
                build_artifact.filepath, build_artifact.localpath, build_artifact.sha256
           FROM build, job
           LEFT JOIN build_artifact ON
             build_artifact.build = build.id AND build.main_binary = build_artifact.filepath
           WHERE job.name = ? AND build.job = job.id
           ORDER BY start_d DESC, start_ps DESC
        |}

  let get_latest =
    Caqti_request.find_opt
      id
      Caqti_type.(tup3
                   id
                   Meta.t
                   file_opt)
      {| SELECT b.id,
           b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
           b.result_kind, b.result_code, b.result_msg,
           b.main_binary, b.job,
           a.filepath, a.localpath, a.sha256
         FROM build b
         LEFT JOIN build_artifact a ON
           a.build = b.id AND b.main_binary = a.filepath
         WHERE b.job = ?
         ORDER BY start_d DESC, start_ps DESC
         LIMIT 1
      |}

  let add =
    Caqti_request.exec
      t
      {| INSERT INTO build
           (uuid, start_d, start_ps, finish_d, finish_ps,
           result_kind, result_code, result_msg, console, script, main_binary, job)
           VALUES
           (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        |}

  let get_by_hash =
    Caqti_request.find_opt
      Rep.cstruct
      (Caqti_type.tup2
         Caqti_type.string
         t)
      {| SELECT job.name,
           b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
           b.result_kind, b.result_code, b.result_msg,
           b.console, b.script, b.main_binary, b.job
         FROM build_artifact a
         INNER JOIN build b ON b.id = a.build
         INNER JOIN job ON job.id = b.job
         WHERE a.sha256 = ?
         ORDER BY b.start_d DESC, b.start_ps DESC
         LIMIT 1
      |}
end

module User = struct
  let migrate =
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

  let rollback =
    Caqti_request.exec
      Caqti_type.unit
      "DROP TABLE IF EXISTS user"

  let get_user =
    Caqti_request.find_opt
      Caqti_type.string
      (Caqti_type.tup2 id user_info)
      {| SELECT id, username, password_hash, password_salt, password_iter
         FROM user
         WHERE username = ?
      |}

  let get_all =
    Caqti_request.collect
      Caqti_type.unit
      Caqti_type.string
      "SELECT username FROM user"

  let add =
    Caqti_request.exec
      user_info
      {| INSERT INTO user (username, password_hash, password_salt, password_iter)
         VALUES (?, ?, ?, ?)
      |}

  let remove =
    Caqti_request.exec
      id
      "DELETE FROM user WHERE id = ?"

  let remove_user =
    Caqti_request.exec
      Caqti_type.string
      "DELETE FROM user WHERE username = ?"

  let update_user =
    Caqti_request.exec
      user_info
      {| UPDATE user
         SET password_hash = ?2,
             password_salt = ?3,
             password_iter = ?4
         WHERE username = ?1
      |}
end

let migrate = [
  Job.migrate;
  Build.migrate;
  Build_artifact.migrate;
  Build_file.migrate;
  User.migrate;
  Caqti_request.exec Caqti_type.unit
    "CREATE INDEX job_build_idx ON build(job)";
  set_current_version;
  set_application_id;
]

let rollback = [
  User.rollback;
  Build_file.migrate;
  Build_artifact.rollback;
  Build.rollback;
  Job.rollback;
  Caqti_request.exec Caqti_type.unit
    "DROP INDEX IF EXISTS job_build_idx";
  Caqti_request.exec Caqti_type.unit
    "PRAGMA user_version = 0";
  Caqti_request.exec Caqti_type.unit
    "PRAGMA application_id = 0";
]

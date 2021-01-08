module Rep = Representation
open Rep

type id = Rep.id

type file = {
  filepath : Fpath.t;
  localpath : Fpath.t;
  sha256 : Cstruct.t;
}

let file =
  let encode { filepath; localpath; sha256 } =
    Ok (filepath, localpath, sha256) in
  let decode (filepath, localpath, sha256) =
    Ok { filepath; localpath; sha256 } in
  Caqti_type.custom ~encode ~decode Caqti_type.(tup3 fpath fpath cstruct)

let last_insert_rowid =
  Caqti_request.find
    Caqti_type.unit
    id
    "SELECT last_insert_rowid()"

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

  let get_by_build_uuid =
    Caqti_request.find_opt
      (Caqti_type.tup2 uuid fpath)
      (Caqti_type.tup2 fpath cstruct)
      {| SELECT build_artifact.localpath, build_artifact.sha256
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
                       string)
                    id)
    in
    let encode { uuid; start; finish; result; console; script; job_id } =
      Ok ((uuid, (start, finish), (result, console), script), job_id)
    in
    let decode ((uuid, (start, finish), (result, console), script), job_id) =
      Ok { uuid; start; finish; result; console; script; job_id }
    in
    Caqti_type.custom ~encode ~decode rep

  module Meta = struct
    type t = {
      uuid : Uuidm.t;
      start : Ptime.t;
      finish : Ptime.t;
      result : Builder.execution_result;
      job_id : id;
    }

    let t =
      let rep =
        Caqti_type.(tup2
                     (tup4
                       uuid
                       Rep.ptime
                       Rep.ptime
                       execution_result)
                     id)
      in
      let encode { uuid; start; finish; result; job_id } =
        Ok ((uuid, start, finish, result), job_id)
      in
      let decode ((uuid, start, finish, result), job_id) =
        Ok { uuid; start; finish; result; job_id }
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
                  console, script, job
           FROM build
           WHERE id = ?
        |}

  let get_by_uuid =
    Caqti_request.find_opt
      Rep.uuid
      (Caqti_type.tup2 id t)
      {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                  result_kind, result_code, result_msg,
                  console, script, job
           FROM build
           WHERE uuid = ?
        |}

  let get_all =
    Caqti_request.collect
      Caqti_type.int64
      (Caqti_type.tup2 id t)
      {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                  result_kind, result_code, result_msg, console,
                  script, job
           FROM build
           WHERE job = ?
           ORDER BY start_d ASC, start_ps ASC
        |}

  let get_all_meta =
    Caqti_request.collect
      Caqti_type.int64
      (Caqti_type.tup2
         id Meta.t)
      {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                  result_kind, result_code, result_msg, job
           FROM build
           WHERE job = ?
           ORDER BY start_d ASC, start_ps ASC
        |}

  let get_all_meta_by_name =
    Caqti_request.collect
      Caqti_type.string
      (Caqti_type.tup2
         id Meta.t)
      {| SELECT build.id, build.uuid,
                  build.start_d, build.start_ps, build.finish_d, build.finish_ps,
                  build.result_kind, build.result_code, build.result_msg, build.job
           FROM build, job
           WHERE job.name = ? AND build.job = job.id
           ORDER BY start_d ASC, start_ps ASC
        |}


  let add =
    Caqti_request.exec
      t
      {| INSERT INTO build
           (uuid, start_d, start_ps, finish_d, finish_ps,
           result_kind, result_code, result_msg, console, script, job)
           VALUES
           (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        |}

end

let migrate = [
  Job.migrate;
  Build.migrate;
  Build_artifact.migrate;
  Build_file.migrate;
]

let rollback = [
  Build_file.migrate;
  Build_artifact.rollback;
  Build.rollback;
  Job.rollback;
]

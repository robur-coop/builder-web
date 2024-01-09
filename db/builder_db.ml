module Rep = Representation
open Rep
open Caqti_request.Infix

let application_id = 1234839235l

(* Please update this when making changes! *)
let current_version = 17L

type 'a id = 'a Rep.id

type file = Rep.file = {
  filepath : Fpath.t;
  sha256 : Cstruct.t;
  size : int;
}

let last_insert_rowid = Rep.last_insert_rowid

let get_application_id =
  Caqti_type.unit ->!  Caqti_type.int32 @@
  "PRAGMA application_id"

let get_version =
  Caqti_type.unit ->!  Caqti_type.int64 @@
  "PRAGMA user_version"

let set_application_id =
  Caqti_type.unit ->. Caqti_type.unit @@
  Printf.sprintf "PRAGMA application_id = %ld" application_id

let set_current_version =
  Caqti_type.unit ->. Caqti_type.unit @@
  Printf.sprintf "PRAGMA user_version = %Ld" current_version

module Job = struct
  let migrate =
    Caqti_type.unit ->. Caqti_type.unit @@
    {| CREATE TABLE job (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         name VARCHAR(255) NOT NULL UNIQUE
       )
    |}

  let rollback =
    Caqti_type.unit ->.  Caqti_type.unit @@
    "DROP TABLE IF EXISTS job"

  let get =
    id `job ->! Caqti_type.string @@
    "SELECT name FROM job WHERE id = ?"

  let get_id_by_name =
    Caqti_type.string ->? id `job @@
    "SELECT id FROM job WHERE name = ?"

  let get_all_with_section_synopsis =
    Caqti_type.unit ->*
    Caqti_type.(tup4 (id `job) string (option string) (option string)) @@
    {| SELECT j.id, j.name, section.value, synopsis.value
       FROM job j, tag section_tag, tag synopsis_tag
       LEFT JOIN job_tag section ON section.job = j.id AND section.tag = section_tag.id
       LEFT JOIN job_tag synopsis ON synopsis.job = j.id AND synopsis.tag = synopsis_tag.id
       WHERE section_tag.tag = 'section' AND synopsis_tag.tag = 'synopsis'
       ORDER BY section.value, j.name ASC
    |}

  let try_add =
    Caqti_type.string ->. Caqti_type.unit @@
    "INSERT OR IGNORE INTO job (name) VALUES (?)"

  let remove =
    id `job ->. Caqti_type.unit @@
    "DELETE FROM job WHERE id = ?"
end

module Tag = struct
  let migrate =
    Caqti_type.unit ->. Caqti_type.unit @@
    {| CREATE TABLE tag (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         tag VARCHAR(255) NOT NULL UNIQUE
       )
    |}

  let rollback =
    Caqti_type.unit ->.  Caqti_type.unit @@
      "DROP TABLE IF EXISTS tag"

  let get_id_by_name =
    Caqti_type.string ->! id `tag @@
      "SELECT id FROM tag WHERE tag = ?"

  let try_add =
    Caqti_type.string ->. Caqti_type.unit @@
    "INSERT OR IGNORE INTO tag (tag) VALUES (?)"
end

module Job_tag = struct
  let migrate =
    Caqti_type.unit ->. Caqti_type.unit @@
    {| CREATE TABLE job_tag (
         id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
         tag INTEGER NOT NULL,
         value TEXT NOT NULL,
         job INTEGER NOT NULL,

         FOREIGN KEY(job) REFERENCES job(id),
         FOREIGN KEY(tag) REFERENCES tag(id),
         UNIQUE(tag, job)
       )
    |}

  let rollback =
    Caqti_type.unit ->. Caqti_type.unit @@
      "DROP TABLE IF EXISTS job_tag"

  let add =
    Caqti_type.(tup3 (id `tag) string (id `job)) ->. Caqti_type.unit @@
    "INSERT INTO job_tag (tag, value, job) VALUES ($1, $2, $3)"

  let update =
    Caqti_type.(tup3 (id `tag) string (id `job)) ->. Caqti_type.unit @@
    "UPDATE job_tag SET value = $2 WHERE tag = $1 AND job = $3"

  let get_value =
    Caqti_type.(tup2 (id `tag) (id `job)) ->?  Caqti_type.string @@
    "SELECT value FROM job_tag WHERE tag = ? AND job = ?"

  let remove_by_job =
    id `job ->. Caqti_type.unit @@
    "DELETE FROM job_tag WHERE job = ?"

end

module Build_artifact = struct
  let migrate =
    Caqti_type.unit ->.  Caqti_type.unit @@
    {| CREATE TABLE build_artifact (
           id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
           filepath TEXT NOT NULL, -- the path as in the build
           sha256 BLOB NOT NULL,
           size INTEGER NOT NULL,
           build INTEGER NOT NULL,

           FOREIGN KEY(build) REFERENCES build(id),
           UNIQUE(build, filepath)
         )
    |}

  let rollback =
    Caqti_type.unit ->. Caqti_type.unit @@
    "DROP TABLE IF EXISTS build_artifact"

  let get =
    id `build_artifact ->! file @@
    {| SELECT filepath, sha256, size
       FROM build_artifact WHERE id = ? |}

  let get_by_build_uuid =
    Caqti_type.tup2 uuid fpath ->? Caqti_type.tup2 (id `build_artifact) file @@
    {| SELECT build_artifact.id, build_artifact.filepath,
         build_artifact.sha256, build_artifact.size
       FROM build_artifact
       INNER JOIN build ON build.id = build_artifact.build
       WHERE build.uuid = ? AND build_artifact.filepath = ?
    |}

  let get_all_by_build =
    id `build ->* Caqti_type.(tup2 (id `build_artifact) file) @@
    "SELECT id, filepath, sha256, size FROM build_artifact WHERE build = ?"

  let exists =
    cstruct ->! Caqti_type.bool @@
    "SELECT EXISTS(SELECT 1 FROM build_artifact WHERE sha256 = ?)"

  let add =
    Caqti_type.(tup2 file (id `build)) ->. Caqti_type.unit @@
    "INSERT INTO build_artifact (filepath, sha256, size, build) \
     VALUES (?, ?, ?, ?)"

  let remove_by_build =
    id `build ->. Caqti_type.unit @@
    "DELETE FROM build_artifact WHERE build = ?"

  let remove =
    id `build_artifact ->. Caqti_type.unit @@
    "DELETE FROM build_artifact WHERE id = ?"
end

module Build = struct
  type t = {
    uuid : Uuidm.t;
    start : Ptime.t;
    finish : Ptime.t;
    result : Builder.execution_result;
    console : Fpath.t;
    script : Fpath.t;
    platform : string;
    main_binary : [`build_artifact] id option;
    input_id : Cstruct.t option;
    user_id : [`user] id;
    job_id : [`job] id;
  }

  let t =
    let rep =
      Caqti_type.(tup3
                    (tup4
                       uuid
                       (tup2
                          Rep.ptime
                          Rep.ptime)
                       (tup2
                          execution_result
                          fpath)
                       (tup4
                          fpath
                          string
                          (option (Rep.id `build_artifact))
                          (option Rep.cstruct)))
                    (id `user)
                    (id `job))
    in
    let encode { uuid; start; finish; result; console; script; platform; main_binary; input_id; user_id; job_id } =
      Ok ((uuid, (start, finish), (result, console), (script, platform, main_binary, input_id)), user_id, job_id)
    in
    let decode ((uuid, (start, finish), (result, console), (script, platform, main_binary, input_id)), user_id, job_id) =
      Ok { uuid; start; finish; result; console; script; platform; main_binary; input_id; user_id; job_id }
    in
    Caqti_type.custom ~encode ~decode rep

  let migrate =
    Caqti_type.unit ->. Caqti_type.unit @@
    {| CREATE TABLE build (
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

  let rollback =
    Caqti_type.unit ->. Caqti_type.unit @@
    "DROP TABLE IF EXISTS build"

  let get_by_uuid =
    Rep.uuid ->?  Caqti_type.tup2 (id `build) t @@
    {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                result_code, result_msg,
                console, script, platform, main_binary, input_id, user, job
         FROM build
         WHERE uuid = ?
    |}

  let get_all =
    id `job ->* Caqti_type.tup2 (id `build) t @@
    {| SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
                result_code, result_msg, console,
                script, platform, main_binary, input_id, user, job
         FROM build
         WHERE job = ?
         ORDER BY start_d DESC, start_ps DESC
    |}

  let get_all_failed =
    Caqti_type.(tup3 int int (option string)) ->* Caqti_type.tup2 Caqti_type.string t @@
    {| SELECT job.name, b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script, b.platform,
         b.main_binary, b.input_id, b.user, b.job
       FROM build b
       INNER JOIN job ON job.id = b.job
       WHERE b.main_binary IS NULL AND ($3 IS NULL OR b.platform = $3)
       ORDER BY start_d DESC, start_ps DESC
       LIMIT $2
       OFFSET $1
    |}

  let get_all_artifact_sha =
    Caqti_type.(tup2 (id `job) (option string)) ->* Rep.cstruct @@
    {| SELECT DISTINCT a.sha256
       FROM build_artifact a, build b
       WHERE b.job = $1 AND b.main_binary = a.id
         AND ($2 IS NULL OR b.platform = $2)
       ORDER BY b.start_d DESC, b.start_ps DESC
    |}

  let get_failed_builds =
    Caqti_type.(tup2 (id `job) (option string)) ->* t @@
    {| SELECT uuid, start_d, start_ps, finish_d, finish_ps,
              result_code, result_msg, console, script,
              platform, main_binary, input_id, user, job
       FROM build
       WHERE job = $1
         AND main_binary IS NULL
         AND ($2 IS NULL OR platform = $2)
       ORDER BY start_d DESC, start_ps DESC
    |}

  let get_latest_successful_with_binary =
    Caqti_type.(tup2 (id `job) string) ->? Caqti_type.tup3 (id `build) t file @@
    {| SELECT b.id,
         b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script,
         b.platform, b.main_binary, b.input_id, b.user, b.job,
         a.filepath, a.sha256, a.size
       FROM build b, build_artifact a
       WHERE b.main_binary = a.id AND b.job = $1 AND b.platform = $2
         AND b.main_binary IS NOT NULL
       ORDER BY b.start_d DESC, b.start_ps DESC
       LIMIT 1
    |}

  let get_latest_successful =
    Caqti_type.(tup2 (id `job) (option string)) ->? t @@
    {| SELECT
         b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script,
         b.platform, b.main_binary, b.input_id, b.user, b.job
       FROM build b
       WHERE b.job = $1
         AND ($2 IS NULL OR b.platform = $2)
         AND b.main_binary IS NOT NULL
       ORDER BY b.start_d DESC, b.start_ps DESC
       LIMIT 1
    |}

  let get_previous_successful_different_output =
    id `build ->? t @@
    {| SELECT
         b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script,
         b.platform, b.main_binary, b.input_id, b.user, b.job
       FROM build b, build b0, build_artifact a, build_artifact a0
       WHERE b0.id = ? AND b0.job = b.job AND
         b.platform = b0.platform AND
         b.main_binary IS NOT NULL AND
         a.id = b.main_binary AND a0.id = b0.main_binary AND
         a.sha256 <> a0.sha256 AND
         (b0.start_d > b.start_d OR b0.start_d = b.start_d AND b0.start_ps > b.start_ps)
       ORDER BY b.start_d DESC, b.start_ps DESC
       LIMIT 1
    |}

  let get_next_successful_different_output =
    id `build ->? t @@
    {| SELECT
         b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script,
         b.platform, b.main_binary, b.input_id, b.user, b.job
       FROM build b, build b0, build_artifact a, build_artifact a0
       WHERE b0.id = ? AND b0.job = b.job AND
         b.platform = b0.platform AND
         b.main_binary IS NOT NULL AND
         a.id = b.main_binary AND a0.id = b0.main_binary AND
         a.sha256 <> a0.sha256 AND
         (b0.start_d < b.start_d OR b0.start_d = b.start_d AND b0.start_ps < b.start_ps)
       ORDER BY b.start_d ASC, b.start_ps ASC
       LIMIT 1
    |}

  let get_same_input_same_output_builds =
    id `build ->* t @@
    {| SELECT b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script,
         b.platform, b.main_binary, b.input_id, b.user, b.job
       FROM build b0, build_artifact a0, build b, build_artifact a
       WHERE b0.id = ? AND a0.id = b0.main_binary AND a0.sha256 = a.sha256
         AND b.main_binary = a.id AND b.id <> b0.id AND b0.input_id = b.input_id
       ORDER BY b.start_d DESC, b.start_ps DESC
    |}

  let get_same_input_different_output_hashes =
    id `build ->* Rep.cstruct @@
    {| SELECT DISTINCT a.sha256
       FROM build b0, build_artifact a0, build b, build_artifact a
       WHERE b0.id = ? AND a0.id = b0.main_binary AND a0.sha256 <> a.sha256
         AND b.main_binary = a.id AND b.id <> b0.id AND b0.input_id = b.input_id
       ORDER BY b.start_d DESC, b.start_ps DESC
    |}

  let get_different_input_same_output_input_ids =
    id `build ->* Rep.cstruct @@
    {| SELECT DISTINCT b.input_id
       FROM build b0, build_artifact a0, build b, build_artifact a
       WHERE b0.id = ? AND a0.id = b0.main_binary AND a0.sha256 = a.sha256
         AND b.main_binary = a.id AND b0.input_id <> b.input_id
    |}

  let get_one_by_input_id =
    Rep.cstruct ->! t @@
    {| SELECT uuid, start_d, start_ps, finish_d, finish_ps,
        result_code, result_msg, console, script,
        platform, main_binary, input_id, user, job
       FROM build
       WHERE input_id = ?
       ORDER BY start_d DESC, start_ps DESC
       LIMIT 1
    |}

  let get_platforms_for_job =
    id `job ->* Caqti_type.string @@
    "SELECT DISTINCT platform FROM build WHERE job = ? ORDER BY platform"

  let add =
    t ->. Caqti_type.unit @@
    {| INSERT INTO build
         (uuid, start_d, start_ps, finish_d, finish_ps,
         result_code, result_msg, console, script, platform, main_binary, input_id, user, job)
         VALUES
         (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |}

  let get_by_hash =
    Rep.cstruct ->! t @@
    {| SELECT
         b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg, b.console, b.script,
         b.platform, b.main_binary, b.input_id, b.user, b.job
       FROM build_artifact a
       INNER JOIN build b ON b.id = a.build
       WHERE a.sha256 = ?
       ORDER BY b.start_d DESC, b.start_ps DESC
       LIMIT 1
    |}

  let get_with_main_binary_by_hash =
    Rep.cstruct ->! Caqti_type.tup2 t file_opt @@
    {| SELECT b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
            b.result_code, b.result_msg, b.console, b.script,
            b.platform, b.main_binary, b.input_id, b.user, b.job,
            a.filepath, a.sha256, a.size
       FROM build_artifact a
       INNER JOIN build b ON b.id = a.build
       WHERE a.sha256 = ?
       ORDER BY b.start_d DESC, b.start_ps DESC
       LIMIT 1
    |}

  let get_with_jobname_by_hash =
    Rep.cstruct ->? Caqti_type.tup2 Caqti_type.string t @@
    {| SELECT job.name,
         b.uuid, b.start_d, b.start_ps, b.finish_d, b.finish_ps,
         b.result_code, b.result_msg,
         b.console, b.script, b.platform, b.main_binary, b.input_id, b.user, b.job
       FROM build_artifact a
       INNER JOIN build b ON b.id = a.build
       INNER JOIN job ON job.id = b.job
       WHERE a.sha256 = ?
       ORDER BY b.start_d DESC, b.start_ps DESC
       LIMIT 1
    |}

  let set_main_binary =
    Caqti_type.tup2 (id `build) (id `build_artifact) ->. Caqti_type.unit @@
    "UPDATE build SET main_binary = $2 WHERE id = $1"

  let remove =
    id `build ->. Caqti_type.unit @@
    "DELETE FROM build WHERE id = ?"
end

module User = struct
  let migrate =
    Caqti_type.unit ->. Caqti_type.unit @@
    {| CREATE TABLE user (
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

  let rollback =
    Caqti_type.unit ->. Caqti_type.unit @@
    "DROP TABLE IF EXISTS user"

  let get_user =
    Caqti_type.string ->? Caqti_type.tup2 (id `user) user_info @@
    {| SELECT id, username, password_hash, password_salt,
         scrypt_n, scrypt_r, scrypt_p, restricted
       FROM user
       WHERE username = ?
    |}

  let get_all =
    Caqti_type.unit ->* Caqti_type.string @@
    "SELECT username FROM user"

  let add =
    user_info ->. Caqti_type.unit @@
    {| INSERT INTO user (username, password_hash, password_salt,
         scrypt_n, scrypt_r, scrypt_p, restricted)
       VALUES (?, ?, ?, ?, ?, ?, ?)
    |}

  let remove_user =
    Caqti_type.string ->. Caqti_type.unit @@
    "DELETE FROM user WHERE username = ?"

  let update_user =
    user_info ->. Caqti_type.unit @@
    {| UPDATE user
       SET password_hash = $2,
           password_salt = $3,
           scrypt_n = $4,
           scrypt_r = $5,
           scrypt_p = $6,
           restricted = $7
       WHERE username = $1
    |}
end

module Access_list = struct
  let migrate =
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

  let rollback =
    Caqti_type.unit ->. Caqti_type.unit @@
    "DROP TABLE IF EXISTS access_list"

  let get =
    Caqti_type.tup2 (id `user) (id `job) ->! id `access_list @@
    "SELECT id FROM access_list WHERE user = ? AND job = ?"

  let add =
    Caqti_type.tup2 (id `user) (id `job) ->. Caqti_type.unit @@
    "INSERT INTO access_list (user, job) VALUES (?, ?)"

  let remove =
    Caqti_type.tup2 (id `user) (id `job) ->. Caqti_type.unit @@
    "DELETE FROM access_list WHERE user = ? AND job = ?"

  let remove_by_job =
    id `job ->. Caqti_type.unit @@
    "DELETE FROM access_list WHERE job = ?"

  let remove_all_by_username =
    Caqti_type.string ->. Caqti_type.unit @@
    {| DELETE FROM access_list
       WHERE access_list.id IN (
         SELECT access_list.id
         FROM access_list
         INNER JOIN user ON access_list.user = user.id
         WHERE user.username = ?
       )
    |}

end

let migrate = [
  Job.migrate;
  Build.migrate;
  Build_artifact.migrate;
  User.migrate;
  Access_list.migrate;
  Tag.migrate;
  Job_tag.migrate;
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_job_start ON build(job, start_d DESC, start_ps DESC)";
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_failed ON build(job, start_d DESC, start_ps DESC) WHERE main_binary IS NULL";
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_input_id ON build(input_id)";
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_main_binary ON build(main_binary)";
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_artifact_sha256 ON build_artifact(sha256)";
  Caqti_type.unit ->. Caqti_type.unit @@
  "CREATE INDEX idx_build_artifact_build ON build_artifact(build)";
  set_current_version;
  set_application_id;
]

let rollback = [
  Job_tag.rollback;
  Tag.rollback;
  Access_list.rollback;
  User.rollback;
  Build_artifact.rollback;
  Build.rollback;
  Job.rollback;
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP INDEX IF EXISTS idx_build_artifact_build";
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP INDEX IF EXISTS idx_build_artifact_sha256";
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP INDEX IF EXISTS idx_build_failed";
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP INDEX IF EXISTS idx_build_input_id";
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP INDEX IF EXISTS idx_build_main_binary";
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP INDEX IF EXISTS idx_build_job_start";
  Caqti_type.unit ->. Caqti_type.unit @@
  "PRAGMA user_version = 0";
  Caqti_type.unit ->. Caqti_type.unit @@
  "PRAGMA application_id = 0";
]

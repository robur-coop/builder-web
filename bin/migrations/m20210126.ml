let new_user_version =
  1L

let set_application_id =
  Caqti_request.exec
    Caqti_type.unit
    (Printf.sprintf "PRAGMA application_id = %ld" Builder_db.application_id)

let alter_build =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    "ALTER TABLE build ADD COLUMN main_binary TEXT"

let all_builds =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    Caqti_type.int64
    "SELECT id FROM build"

let bin_artifact =
  Caqti_request.collect ~oneshot:true
    Caqti_type.int64
    Caqti_type.(tup2 int64 string)
    "SELECT id, filepath FROM build_artifact WHERE build = ? AND filepath LIKE 'bin/%'"

let set_main_binary =
  Caqti_request.exec ~oneshot:true
    Caqti_type.(tup2 int64 (option string))
    "UPDATE build SET main_binary = ?2 WHERE id = ?1"

let migrate (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~application_id:0l ~user_version:0L (module Db) >>= fun () ->
  Db.exec alter_build () >>= fun () ->
  Db.collect_list all_builds () >>= fun builds ->
  Grej.list_iter_result (fun build ->
      Db.collect_list bin_artifact build >>= function
      | [_id, main_binary] ->
        Db.exec set_main_binary (build, Some main_binary)
      | [] ->
        Logs.debug (fun m -> m "No binaries for build id %Ld" build);
        Ok ()
      | binaries ->
        Logs.warn (fun m -> m "More than one binary for build id %Ld" build);
        Logs.debug (fun m -> m "binaries: [%a]" Fmt.(list ~sep:(any ";") string)
                       (List.map snd binaries));
        Ok ())
    builds >>= fun () ->
  Db.exec Builder_db.set_application_id () >>= fun () ->
  Db.exec (Grej.set_version new_user_version) ()

let rename_build =
  Caqti_request.exec
    Caqti_type.unit
    "ALTER TABLE build RENAME TO __tmp_build"

let create_build =
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

let rollback_data =
  Caqti_request.exec
    Caqti_type.unit
    {| INSERT INTO build
       SELECT id, uuid, start_d, start_ps, finish_d, finish_ps,
         result_kind, result_code, result_msg, console, script, job
       FROM __tmp_build
    |}

let rollback (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:new_user_version (module Db) >>= fun () ->
  Db.exec rename_build () >>= fun () ->
  Db.exec create_build () >>= fun () ->
  Db.exec rollback_data () >>= fun () ->
  Db.exec (Grej.set_version 0L) ()

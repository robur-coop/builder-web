let new_version = 14L and old_version = 13L
and identifier = "2021-07-12c"
and migrate_doc = "store script, console on disk"
and rollback_doc = "store script, console in database"

open Grej.Infix

module Asn = struct
  let decode_strict codec cs =
    match Asn.decode codec cs with
    | Ok (a, rest) ->
      if String.length rest = 0
      then Ok a
      else Error "trailing bytes"
    | Error (`Parse msg) -> Error ("parse error: " ^ msg)

  let projections_of asn =
    let c = Asn.codec Asn.der asn in
    (decode_strict c, Asn.encode c)

  let console =
    Asn.S.(sequence_of
             (sequence2
                (required ~label:"delta" int)
                (required ~label:"data" utf8_string)))

  let console_of_cs, console_to_cs = projections_of console
end

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

let copy_from_old_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| INSERT INTO new_build(id, uuid, start_d, start_ps, finish_d, finish_ps,
       result_code, result_msg, console, script, main_binary, user, job, input_id)
     SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_code, result_msg,
       '', '', main_binary, user, job, input_id
     FROM build
  |}

let copy_from_new_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| INSERT INTO new_build(id, uuid, start_d, start_ps, finish_d, finish_ps,
       result_code, result_msg, console, script, main_binary, user, job, input_id)
     SELECT id, uuid, start_d, start_ps, finish_d, finish_ps, result_code, result_msg,
       x'', '', main_binary, user, job, input_id
     FROM build
  |}

let old_build_console_script =
  Caqti_type.unit ->*
  Caqti_type.(t4 (Builder_db.Rep.id  (`build : [ `build ]))
                (t2 string Builder_db.Rep.uuid) octets string) @@
  "SELECT b.id, job.name, b.uuid, b.console, b.script FROM build b, job WHERE b.job = job.id"

let update_new_build_console_script =
  Caqti_type.(t3 (Builder_db.Rep.id (`build : [ `build ]))
                Builder_db.Rep.fpath Builder_db.Rep.fpath) ->.
  Caqti_type.unit @@
  "UPDATE new_build SET console = $2, script = $3 WHERE id = $1"

let new_build_console_script =
  Caqti_type.unit ->*
  Caqti_type.t3 (Builder_db.Rep.id (`build : [ `build ]))
    Builder_db.Rep.fpath Builder_db.Rep.fpath @@
  "SELECT id, console, script FROM build"

let update_old_build_console_script =
  Caqti_type.(t3 (Builder_db.Rep.id (`build : [ `build ])) octets string) ->.
  Caqti_type.unit @@
  "UPDATE new_build SET console = $2, script = $3 WHERE id = $1"

let drop_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE build"

let rename_build =
  Caqti_type.unit ->. Caqti_type.unit @@
  "ALTER TABLE new_build RENAME TO build"

let console_to_string console =
  Asn.console_of_cs console
  |> Result.map_error (fun s -> `Msg s) >>| fun console ->
  List.rev_map (fun (delta, data) ->
      Printf.sprintf "%.3fs:%s\n" (Duration.to_f (Int64.of_int delta)) data)
    console
  |> String.concat ""

let console_of_string data =
  let lines = String.split_on_char '\n' data in
  (* remove last empty line *)
  let lines =
    match List.rev lines with
    | "" :: lines -> List.rev lines
    | _ -> lines
  in
  let console = List.map (fun line ->
      match String.split_on_char ':' line with
      | ts :: tail ->
        let delta = float_of_string (String.sub ts 0 (String.length ts - 1)) in
        Int64.to_int (Duration.of_f delta), String.concat ":" tail
      | _ -> assert false)
      lines
  in
  Asn.console_to_cs console

let save_console_and_script datadir job_name uuid console script =
  let out name = Fpath.(v job_name / Uuidm.to_string uuid / name + "txt") in
  let script_out = out "script" in
  Bos.OS.File.write Fpath.(datadir // script_out) script >>= fun () ->
  let console_out = out "console" in
  console_to_string console >>= fun console_data ->
  Bos.OS.File.write Fpath.(datadir // console_out) console_data >>= fun () ->
  Ok (console_out, script_out)

let read_console_and_script datadir console_file script_file =
  let console_file = Fpath.append datadir console_file
  and script_file = Fpath.append datadir script_file
  in
  Bos.OS.File.read console_file >>= fun console ->
  Bos.OS.File.read script_file >>= fun script ->
  let console = console_of_string console in
  Bos.OS.File.delete console_file >>= fun () ->
  Bos.OS.File.delete script_file >>= fun () ->
  Ok (console, script)

let migrate datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec new_build () >>= fun () ->
  Db.exec copy_from_old_build () >>= fun () ->
  Db.collect_list old_build_console_script () >>= fun console_scripts ->
  Grej.list_iter_result (fun (id, (job_name, uuid), console, script) ->
      save_console_and_script datadir job_name uuid console script >>= fun (console_file, script_file) ->
      Db.exec update_new_build_console_script (id, console_file, script_file))
    console_scripts >>= fun () ->
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
  Db.exec (Grej.set_version new_version) ()

let rollback datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec old_build () >>= fun () ->
  Db.exec copy_from_new_build () >>= fun () ->
  Db.collect_list new_build_console_script () >>= fun console_scripts ->
  Grej.list_iter_result (fun (id, console_file, script_file) ->
      read_console_and_script datadir console_file script_file >>= fun (console, script) ->
      Db.exec update_old_build_console_script (id, console, script))
    console_scripts >>= fun () ->
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
  Db.exec (Grej.set_version old_version) ()

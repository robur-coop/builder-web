let new_version = 10L and old_version = 9L
let identifier = "2021-06-30"
let migrate_doc = "add readme.md tag"
let rollback_doc = "remove readme.md tag"

open Grej.Infix

let jobs =
  Caqti_type.unit ->* Builder_db.Rep.untyped_id @@
  "SELECT id FROM job"

let latest_successful_build =
  Builder_db.Rep.untyped_id ->? Builder_db.Rep.untyped_id @@
  {| SELECT b.id
     FROM build b
     WHERE b.job = ? AND b.result_kind = 0 AND b.result_code = 0
     ORDER BY b.start_d DESC, b.start_ps DESC
     LIMIT 1
  |}

let build_artifacts =
  Builder_db.Rep.untyped_id ->*
  Caqti_type.t2 Builder_db.Rep.fpath Builder_db.Rep.fpath @@
  {| SELECT a.filepath, a.localpath
     FROM build_artifact a
     WHERE a.build = ?
  |}

let insert_tag =
  Caqti_type.string ->. Caqti_type.unit @@
  "INSERT INTO tag (tag) VALUES (?)"

let insert_job_tag =
  Caqti_type.(t3 Builder_db.Rep.untyped_id string Builder_db.Rep.untyped_id) ->.
  Caqti_type.unit @@
  "INSERT INTO job_tag (tag, value, job) VALUES (?, ?, ?)"

let find_tag =
  Caqti_type.string ->! Builder_db.Rep.untyped_id @@
  "SELECT id FROM tag where tag = ?"

let remove_job_tag =
  Builder_db.Rep.untyped_id ->. Caqti_type.unit @@
  "DELETE FROM job_tag where tag = ?"

let remove_tag =
  Builder_db.Rep.untyped_id ->. Caqti_type.unit @@
  "DELETE FROM tag where id = ?"

let migrate datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec insert_tag "readme.md" >>= fun () ->
  Db.find find_tag "readme.md" >>= fun readme_id ->
  Db.collect_list jobs () >>= fun jobs ->
  Grej.list_iter_result (fun job ->
    Db.find_opt latest_successful_build job >>= function
    | None -> Ok ()
    | Some build ->
      Db.collect_list build_artifacts build >>= fun artifacts ->
      List.fold_left (fun acc (fpath, lpath) ->
          acc >>= fun acc ->
          Bos.OS.File.read Fpath.(append datadir lpath) >>= fun data ->
          Ok ((fpath, data) :: acc))
        (Ok [])
        artifacts >>= fun files ->
      let readme =
        List.find_opt (fun (p, _) -> Fpath.(equal (v "README.md") p)) files
      in
      let readme_anywhere =
        List.find_opt (fun (p, _) -> String.equal "README.md" (Fpath.basename p)) files
      in
      (match readme, readme_anywhere with
       | None, None -> Ok ()
       | Some (_, data), _ | None, Some (_, data) ->
         Db.exec insert_job_tag (readme_id, data, job)))
    jobs >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.find find_tag "readme.md" >>= fun readme_id ->
  Db.exec remove_job_tag readme_id >>= fun () ->
  Db.exec remove_tag readme_id >>= fun () ->
  Db.exec (Grej.set_version old_version) ()




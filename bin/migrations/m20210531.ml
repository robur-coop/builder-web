let old_version = 3L
let new_version = 4L
let identifier = "2021-05-31"
let migrate_doc = "remove datadir prefix from build_artifact.localpath"
let rollback_doc = "add datadir prefix to build_artifact.localpath"

open Grej.Infix

let build_artifacts =
  Caqti_type.unit ->* Caqti_type.t2 Builder_db.Rep.untyped_id Builder_db.Rep.fpath @@
  "SELECT id, localpath FROM build_artifact"

let build_artifact_update_localpath =
  Caqti_type.t2 Builder_db.Rep.untyped_id Builder_db.Rep.fpath ->. Caqti_type.unit @@
    "UPDATE build_artifact SET localpath = $2 WHERE id = $1"

(* We are not migrating build_file because it is unused *)

let migrate datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.collect_list build_artifacts () >>= fun artifacts ->
  Grej.list_iter_result (fun (id, localpath) ->
    match Fpath.rem_prefix datadir localpath with
    | Some p -> Db.exec build_artifact_update_localpath (id, p)
    | None -> Error (`Msg ("couldn't remove datadir prefix from " ^ Fpath.to_string localpath)))
   artifacts >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
   Db.collect_list build_artifacts () >>= fun artifacts ->
  Grej.list_iter_result (fun (id, localpath) ->
    let p = Fpath.(datadir // localpath) in
    Db.exec build_artifact_update_localpath (id, p))
   artifacts >>= fun () ->
  Db.exec (Grej.set_version old_version) ()

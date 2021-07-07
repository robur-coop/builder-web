
let all_builds_with_binary : (unit, [`build] Builder_db.Rep.id * [`build_artifact] Builder_db.Rep.id * Fpath.t * Fpath.t, [ `Zero | `One | `Many ]) Caqti_request.t =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup4 (Builder_db.Rep.id `build) (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath Builder_db.Rep.fpath)
    "SELECT b.id, b.main_binary, a.filepath, a.localpath FROM build b, build_artifact a WHERE b.main_binary = a.id AND b.main_binary IS NOT NULL"

let build_not_stripped : ([`build] Builder_db.Rep.id, [`build_artifact] Builder_db.Rep.id, [< `Zero | `One | `Many > `Zero `One ]) Caqti_request.t =
  Caqti_request.find_opt
    (Builder_db.Rep.id `build)
    (Builder_db.Rep.id `build_artifact)
    {| SELECT id FROM build_artifact WHERE build = ? AND filepath LIKE '%.debug' |}

let update_paths : ([`build_artifact] Builder_db.Rep.id * Fpath.t * Fpath.t, unit, [< `Zero | `One | `Many > `Zero ]) Caqti_request.t =
  Caqti_request.exec
    (Caqti_type.tup3 (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath Builder_db.Rep.fpath)
    "UPDATE build_artifact SET localpath = ?2, filepath = ?3 WHERE id = ?1"

let add_artifact : ((Fpath.t * Fpath.t * Cstruct.t) * (int64 * [`build] Builder_db.Rep.id), unit, [< `Zero | `One | `Many > `Zero]) Caqti_request.t =
  Caqti_request.exec
    Caqti_type.(tup2 (tup3 Builder_db.Rep.fpath Builder_db.Rep.fpath Builder_db.Rep.cstruct) (tup2 Caqti_type.int64 (Builder_db.Rep.id `build)))
    {| INSERT INTO build_artifact (filepath, localpath, sha256, size, build) VALUES (?, ?, ?, ?, ?) |}

let fixup datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:12L (module Db) >>= fun () ->
  Db.rev_collect_list all_builds_with_binary () >>= fun builds ->
  Grej.list_iter_result
    (fun (build_id, artifact_id, artifact_fpath, artifact_lpath) ->
       if Fpath.has_ext ".hvt" artifact_fpath || Fpath.has_ext ".xen" artifact_fpath then
         Db.find_opt build_not_stripped build_id >>= fun stripped_id ->
         if stripped_id = None then begin
           Logs.info (fun m -> m "artifact (not stripped) %a" Fpath.pp artifact_lpath);
           let path p =
             let fname = Fpath.(filename p) in
             let dir = Fpath.(parent (parent p)) in
             Fpath.(dir / fname + "debug")
           in
           let new_artifact_lpath = path artifact_lpath in
           let r =
             Sys.command (Printf.sprintf "cp %s %s"
                           (Fpath.to_string (Fpath.append datadir artifact_lpath))
                           (Fpath.to_string (Fpath.append datadir new_artifact_lpath)))
           in
           assert (r = 0);
           let r =
             Sys.command (Printf.sprintf "strip %s" (Fpath.to_string (Fpath.append datadir artifact_lpath)))
           in
           assert (r = 0);
           Bos.OS.File.read (Fpath.append datadir artifact_lpath) >>= fun data ->
           let size = Int64.of_int (String.length data) and sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
           Db.exec update_paths (artifact_id, new_artifact_lpath, path artifact_fpath) >>= fun () ->
           Db.exec add_artifact ((artifact_fpath, artifact_lpath, sha256), (size, build_id)) >>= fun () ->
           Db.find Builder_db.last_insert_rowid () >>= fun new_build_artifact_id ->
           Db.exec Builder_db.Build.set_main_binary (build_id, new_build_artifact_id)
         end else
           Ok ()
       else Ok ())
    builds

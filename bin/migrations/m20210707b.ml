open Grej.Infix

let deb_debug_left_in_builds =
  Caqti_type.unit ->*
  Caqti_type.tup4 (Builder_db.Rep.id `build_artifact) (Builder_db.Rep.id `build)
    Builder_db.Rep.fpath Builder_db.Rep.fpath @@
  {| SELECT id, build, localpath, filepath FROM build_artifact
     WHERE filepath LIKE '%.deb.debug'
  |}

let get_main_binary =
  Builder_db.Rep.id `build ->? Builder_db.Rep.id `build_artifact @@
  "SELECT main_binary FROM build WHERE id = ?"

let get_localpath =
  Builder_db.Rep.id `build_artifact ->! Builder_db.Rep.fpath @@
  "SELECT localpath FROM build_artifact WHERE id = ?"

let update_paths =
  Caqti_type.tup3 (Builder_db.Rep.id `build_artifact)
    Builder_db.Rep.fpath Builder_db.Rep.fpath ->.
  Caqti_type.unit @@
  "UPDATE build_artifact SET localpath = $2, filepath = $3 WHERE id = $1"

let fixup datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:12L (module Db) >>= fun () ->
  Db.rev_collect_list deb_debug_left_in_builds () >>= fun leftover_debug ->
  Grej.list_iter_result
    (fun (id, build_id, path, fpath) ->
       (Db.find_opt get_main_binary build_id >>= function
        | None -> Ok (fun () -> Ok ())
        | Some main_id ->
          Db.find get_localpath main_id >>= fun lpath ->
          Logs.info (fun m -> m "deleting %a" Fpath.pp lpath);
          Bos.OS.File.delete (Fpath.append datadir lpath) >>= fun () ->
          Ok (fun () -> Db.exec Builder_db.Build_artifact.remove main_id)) >>= fun later ->
       Db.exec Builder_db.Build.set_main_binary (build_id, id) >>= fun () ->
       let new_path p =
         let fname = Fpath.(filename (rem_ext p)) in
         let dir = Fpath.(parent p) in
         Fpath.(dir / "bin" / fname)
       in
       Db.exec update_paths (id, new_path path, new_path fpath) >>= fun () ->
       let o = Fpath.append datadir path and n = Fpath.append datadir (new_path path) in
       Logs.info (fun m -> m "renaming %a to %a" Fpath.pp o Fpath.pp n);
       Result.map_error (fun e -> `Msg (Fmt.str "%a" Bos.OS.U.pp_error e))
         (Bos.OS.U.rename o n) >>= fun () ->
       later ())
    leftover_debug


let all_build_artifacts_with_dot_slash : (unit, [`build_artifact] Builder_db.Rep.id * Fpath.t, [ `Zero | `One | `Many ]) Caqti_request.t =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup2 (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath)
    "SELECT id, filepath FROM build_artifact WHERE filepath LIKE './%'"

let update_path : ([`build_artifact] Builder_db.Rep.id * Fpath.t, unit, [< `Zero | `One | `Many > `Zero ]) Caqti_request.t =
  Caqti_request.exec
    (Caqti_type.tup2 (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath)
    "UPDATE build_artifact SET filepath = ?2 WHERE id = ?1"

let fixup _datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Rresult.R.Infix in
  Grej.check_version ~user_version:12L (module Db) >>= fun () ->
  Db.rev_collect_list all_build_artifacts_with_dot_slash () >>= fun build_artifacts ->
  Grej.list_iter_result
    (fun (id, fpath) ->
       let segs = match Fpath.segs fpath with
       | "." :: tl -> tl
       | x -> x
       in
       let fpath' = Fpath.v (String.concat "/" segs) in
       if Fpath.equal fpath fpath' then
         Ok ()
       else
         Db.exec update_path (id, fpath'))
    build_artifacts

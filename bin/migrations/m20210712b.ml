
let all_build_artifacts_like_hashes : (unit, [`build_artifact] Builder_db.Rep.id * Fpath.t, [ `Zero | `One | `Many ]) Caqti_request.t =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup2 (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath)
    "SELECT id, localpath FROM build_artifact WHERE filepath LIKE '%.build-hashes'"

let all_build_artifacts_like_readme : (unit, [`build_artifact] Builder_db.Rep.id * Fpath.t, [ `Zero | `One | `Many ]) Caqti_request.t =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup2 (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath)
    "SELECT id, localpath FROM build_artifact WHERE filepath LIKE '%README.md'"

let fixup datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Grej.Infix in
  Grej.check_version ~user_version:13L (module Db) >>= fun () ->
  Db.rev_collect_list all_build_artifacts_like_hashes () >>= fun build_artifacts_build_hashes ->
  Db.rev_collect_list all_build_artifacts_like_readme () >>= fun build_artifacts_readme ->
  Grej.list_iter_result
    (fun (artifact_id, artifact_lpath) ->
        Bos.OS.File.delete (Fpath.append datadir artifact_lpath) >>= fun () ->
        Db.exec Builder_db.Build_artifact.remove artifact_id)
    (build_artifacts_build_hashes @ build_artifacts_readme)

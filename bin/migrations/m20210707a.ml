open Grej.Infix

let orb_left_in_builds =
  Caqti_type.unit ->*
  Caqti_type.t2 (Builder_db.Rep.id `build_artifact) Builder_db.Rep.fpath @@
  {| SELECT id, localpath FROM build_artifact
     WHERE filepath = 'orb.deb' OR filepath = 'orb.txz'
  |}

let fixup datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:12L (module Db) >>= fun () ->
  Db.rev_collect_list orb_left_in_builds () >>= fun leftover_orb ->
  Grej.list_iter_result
    (fun (id, path) ->
       Bos.OS.File.delete (Fpath.append datadir path) >>= fun () ->
       Db.exec Builder_db.Build_artifact.remove id)
    leftover_orb

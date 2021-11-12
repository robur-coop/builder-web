module Rep = Builder_db.Rep

let broken_builds =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup3 (Rep.id `build) Rep.uuid Caqti_type.string)
    {| SELECT b.id, b.uuid, job.name FROM build b, job
       WHERE result_kind = 0 AND result_code = 0 AND main_binary IS NOT NULL AND job.id = b.job AND
         (SELECT COUNT( * ) FROM build_artifact a
          WHERE a.build = b.id and a.filepath = b.main_binary) = 0
    |}

let fixup datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Grej.Infix in
  Grej.check_version ~user_version:3L (module Db) >>= fun () ->
  Db.rev_collect_list broken_builds () >>= fun broken_builds ->
  Grej.list_iter_result
    (fun ((build, uuid, job_name) : [`build] Rep.id * Uuidm.t * string) ->
       Format.printf "Removing job %a.\nPlease clean up data files in %a/%s/%a\n"
         Uuidm.pp uuid Fpath.pp datadir job_name Uuidm.pp uuid;
       Db.exec Builder_db.Build.remove build)
    broken_builds

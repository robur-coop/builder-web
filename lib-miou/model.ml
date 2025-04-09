let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

module type CONN = Caqti_miou.CONNECTION

let jobs_with_section_synopsis (module Db : CONN) =
  Db.collect_list Builder_db.Job.get_all_with_section_synopsis ()

let platforms_of_job id (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_platforms_for_job id

let build_with_main_binary job platform (module Db : CONN) =
  let+ opt = Db.find_opt Builder_db.Build.get_latest_successful_with_binary (job, platform) in
  Option.map (fun (_id, build, file) -> (build, file)) opt

let build_hash hash (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_with_jobname_by_hash hash

let build_exists uuid (module Db : CONN) =
  let+ opt = Db.find_opt Builder_db.Build.get_by_uuid uuid in
  Option.is_some opt

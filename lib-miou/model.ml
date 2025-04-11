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

let builds_grouped_by_output job_id platform (module Db : CONN) =
  let* lst = Db.collect_list Builder_db.Build.get_all_artifact_sha (job_id, platform) in
  let fn acc hash = match acc with
    | Error _ as err -> err
    | Ok builds ->
        let* build = Db.find Builder_db.Build.get_with_main_binary_by_hash hash in
        Ok (build :: builds) in
  let* lst = List.fold_left fn (Ok []) lst in
  Ok (List.rev lst)

let builds_grouped_by_output_with_failed job_id platform ((module Db : CONN) as db) =
  let+ builds = builds_grouped_by_output job_id platform db in
  let+ failed = Db.collect_list Builder_db.Build.get_failed_builds (job_id, platform) in
  let failed = List.map (fun b -> b, None) failed in
  let cmp (a, _) (b, _) = Ptime.compare b.Builder_db.Build.start a.Builder_db.Build.start in
  List.merge cmp builds failed

let not_found = function
  | None -> Error `Not_found
  | Some v -> Ok v

let job_id job_name (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name

let job_and_readme job (module Db : CONN) =
  let* job_id = job_id job (module Db) in
  let* job_id = not_found job_id in
  let* readme_id = Db.find Builder_db.Tag.get_id_by_name "readme.md" in
  let* readme = Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id) in
  Ok (job_id, readme)

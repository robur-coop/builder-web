let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let not_found = function
  | None -> Error `Not_found
  | Some v -> Ok v

let staging datadir = Fpath.(datadir / "_staging")
let artifact_path artifact =
  let sha256 = Ohex.encode artifact.Builder_db.sha256 in
  (* NOTE: [sha256] is 64 characters when it's a hex sha256 checksum *)
  (* NOTE: We add the prefix to reduce the number of files in a directory - a
     workaround for inferior filesystems. We can easily revert this by changing
     this function and adding a migration. *)
  let prefix = String.sub sha256 0 2 in
  Fpath.(v "_artifacts" / prefix / sha256)

module type CONN = Caqti_miou.CONNECTION

let build_artifact build filepath (module Db : CONN) =
  let* artifact = Db.find_opt Builder_db.Build_artifact.get_by_build_uuid (build, filepath) in
  let* (_id, artifact) = not_found artifact in
  Ok artifact

let build_artifact_by_id id (module Db : CONN) =
  Db.find Builder_db.Build_artifact.get id

let build_artifact_path datadir file =
  Fpath.(datadir // artifact_path file)

let build_artifacts build (module Db : CONN) =
  let+ artifacts = Db.collect_list Builder_db.Build_artifact.get_all_by_build build in
  List.map snd artifacts

let solo5_manifest datadir file =
  let cachet =
    let path = Fpath.(to_string (datadir // artifact_path file)) in
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    let len = Unix.lseek fd 0 Unix.SEEK_END in
    let buf =
      Bigarray.array1_of_genarray
        (Unix.map_file fd Bigarray.char
           Bigarray.c_layout false [|len|]) in
    Unix.close fd;
    let map () ~pos len =
      let len = min len (max 0 (Bigarray.Array1.dim buf - pos)) in
      Bigarray.Array1.sub buf pos len
    in
    Cachet.make ~pagesize:8 ~map ()
  in
  Solo5_elftool.query_manifest cachet |> Result.to_option

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
  let* builds = builds_grouped_by_output job_id platform db in
  let* failed = Db.collect_list Builder_db.Build.get_failed_builds (job_id, platform) in
  let failed = List.map (fun b -> b, None) failed in
  let cmp (a, _) (b, _) = Ptime.compare b.Builder_db.Build.start a.Builder_db.Build.start in
  Result.ok (List.merge cmp builds failed)

let build uuid (module Db : CONN) =
  let* build = Db.find_opt Builder_db.Build.get_by_uuid uuid in
  not_found build

let builds_with_same_input_and_same_main_binary id (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_same_input_same_output_builds id

let builds_with_same_input_and_different_main_binary id (module Db : CONN) =
  let* hashes = Db.collect_list Builder_db.Build.get_same_input_different_output_hashes id in
  List.fold_left (fun acc hash ->
     match acc with
     | Error _ as e -> e
     | Ok metas ->
       let* build = Db.find Builder_db.Build.get_by_hash hash in
       Ok (build :: metas))
   (Ok []) hashes

let builds_with_different_input_and_same_main_binary id (module Db : CONN) =
  let* ids = Db.collect_list Builder_db.Build.get_different_input_same_output_input_ids id in
  List.fold_left (fun acc input_id ->
     match acc with
     | Error _ as e -> e
     | Ok metas ->
       let* build = Db.find Builder_db.Build.get_one_by_input_id input_id in
       Ok (build :: metas))
   (Ok []) ids

let next_successful_build_different_output id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_next_successful_different_output id

let previous_successful_build_different_output id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_previous_successful_different_output id

let job_id job_name (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name

let job_and_readme job (module Db : CONN) =
  let* job_id = job_id job (module Db) in
  let* job_id = not_found job_id in
  let* readme_id = Db.find Builder_db.Tag.get_id_by_name "readme.md" in
  let* readme = Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id) in
  Ok (job_id, readme)

let latest_successful_build job_id platform (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest_successful (job_id, platform)

let latest_successful_build_uuid job_id platform db =
  let* build = latest_successful_build job_id platform db in
  Option.map (fun build -> build.Builder_db.Build.uuid) build |> Result.ok

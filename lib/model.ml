let src = Logs.Src.create "builder-web.model" ~doc:"Builder-web model"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let pp_error ppf = function
  | `Not_found -> Format.fprintf ppf "value not found in database"
  | `File_error path -> Format.fprintf ppf "error reading file %a" Fpath.pp path
  | `Msg e -> Format.fprintf ppf "error %s" e
  | #Caqti_error.t as e ->
    Caqti_error.pp ppf e

let not_found = function
  | None -> Error `Not_found
  | Some v -> Ok v

let staging datadir = Fpath.(datadir / "_staging")
let cachedir datadir = Fpath.(datadir / "_cache")
let artifact_path artifact =
  let sha256 = Ohex.encode artifact.Builder_db.sha256 in
  (* NOTE: [sha256] is 64 characters when it's a hex sha256 checksum *)
  (* NOTE: We add the prefix to reduce the number of files in a directory - a
     workaround for inferior filesystems. We can easily revert this by changing
     this function and adding a migration. *)
  let prefix = String.sub sha256 0 2 in
  Fpath.(v "_artifacts" / prefix / sha256)

let read_file datadir filepath =
  let filepath = Fpath.(datadir // filepath) in
  match
    let fd = Unix.openfile (Fpath.to_string filepath) [Unix.O_RDONLY] 0 in
    fd, Unix.fstat fd
  with
  | fd, { Unix.st_size; _ } ->
    Fun.protect
      ~finally:(fun () -> try Unix.close fd with Unix_error _ -> ())
    @@ fun () ->
    let buf = Bytes.create st_size in
    let rec loop last_yield off =
      let last_yield =
        (* yield if we've read at least 128 kb since last yield *)
        if off - last_yield >= 0x2000 then
          (Miou.yield (); off)
        else last_yield
      in
      match Unix.read fd buf off (min 0x7ff (st_size - off)) with
      | 0 ->
        Ok (Bytes.unsafe_to_string buf)
      | l ->
        loop last_yield (off + l)
      | exception Unix.Unix_error _ ->
        Error (`File_error filepath)
    in
    loop 0 0
  | exception Unix.Unix_error _ ->
    Error (`File_error filepath)

let save path data =
  try
    let fd = Unix.openfile (Fpath.to_string path) [Unix.O_WRONLY; Unix.O_CREAT] 0o666 in
    let rec loop off =
      if off = String.length data then
        Ok ()
      else
        let l = Unix.write_substring fd data off (String.length data - off) in
        loop (off + l)
    in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> loop 0)
  with Unix.Unix_error _ -> Error (`File_error path)

let save_artifact staging artifact data =
  let sha256 = Ohex.encode artifact.Builder_db.sha256 in
  let destpath = Fpath.(staging / sha256) in
  save destpath data

module type CONN = Caqti_miou.CONNECTION

let build_artifact build filepath (module Db : CONN) =
  let* artifact = Db.find_opt Builder_db.Build_artifact.get_by_build_uuid (build, filepath) in
  let* (_id, artifact) = not_found artifact in
  Ok artifact

let build_artifact_by_id id (module Db : CONN) =
  Db.find Builder_db.Build_artifact.get id

let build_artifact_data datadir build filepath conn =
  let* artifact = build_artifact build filepath conn in
  read_file datadir (artifact_path artifact)

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

let failed_builds ~start ~count platform (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_all_failed (start, count, platform)

let build_console_by_uuid datadir uuid (module Db : CONN) =
  let* (_id, { Builder_db.Build.console; _ }) = build uuid (module Db) in
  Ok (Fpath.(datadir // console))

let build_script_by_uuid datadir uuid (module Db : CONN) =
  let* (_id, { Builder_db.Build.script; _ }) = build uuid (module Db) in
  Ok (Fpath.(datadir // script))

let readme job_id (module Db : CONN) =
  let* readme_id = Db.find_opt Builder_db.Tag.get_id_by_name "readme.md" in
  match readme_id with
  | None -> Ok None
  | Some readme_id ->
    Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id)

let job_id job_name (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name

let job_name job_id (module Db : CONN) =
  Db.find Builder_db.Job.get job_id

let job_and_readme job (module Db : CONN) =
  let* job_id = job_id job (module Db) in
  let* job_id = not_found job_id in
  let* readme = readme job_id (module Db) in
  Ok (job_id, readme)

let latest_successful_build job_id platform (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest_successful (job_id, platform)

let latest_successful_build_uuid job_id platform db =
  let* build = latest_successful_build job_id platform db in
  Option.map (fun build -> build.Builder_db.Build.uuid) build |> Result.ok

let user username (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username

let authorized user_id user job_name (module Db : CONN) =
  if user.Builder_web_auth.restricted then
    let* r = job_id job_name (module Db) in
    match r with
    | None -> Ok false
    | Some job_id ->
      let* r = Db.find_opt Builder_db.Access_list.get (user_id, job_id) in
      Ok (Option.is_some r)
  else Ok true

(* Quite a few helpers for [add_build]: *)

let commit_files datadir staging_dir job_name uuid artifacts =
  (* First we move the artifacts *)
  let* () =
    List.fold_left
      (fun r artifact ->
         let* () = r in
         let sha256 = Ohex.encode artifact.Builder_db.sha256 in
         let src = Fpath.(staging_dir / sha256) in
         let dest = Fpath.(datadir // artifact_path artifact) in
         let* _created = Bos.OS.Dir.create (Fpath.parent dest) in
         Bos.OS.Path.move ~force:true src dest)
      (Ok ())
      artifacts
  in
  (* Now the staging dir only contains script & console *)
  let job_dir = Fpath.(datadir / job_name) in
  let dest = Fpath.(job_dir / Uuidm.to_string uuid) in
  let* _created = (Bos.OS.Dir.create job_dir) in
  Bos.OS.Path.move staging_dir dest

let infer_section_and_synopsis artifacts =
  let infer_synopsis_and_descr switch root =
    match OpamPackage.Name.Map.find_opt root.OpamPackage.name switch.OpamFile.SwitchExport.overlays with
    | None -> None, None
    | Some opam -> OpamFile.OPAM.synopsis opam, OpamFile.OPAM.descr_body opam
  in
  let infer_section switch root =
    let root_pkg = root.OpamPackage.name in
    let is_unikernel =
      (* since mirage 4.2.0, the x-mirage-opam-lock-location is emitted *)
      Option.value ~default:false
        (Option.map (fun opam ->
             Option.is_some (OpamFile.OPAM.extended opam "x-mirage-opam-lock-location" Fun.id))
            (OpamPackage.Name.Map.find_opt root_pkg switch.OpamFile.SwitchExport.overlays))
    in
    let root_pkg_name = OpamPackage.Name.to_string root_pkg in
    if is_unikernel || Astring.String.is_prefix ~affix:"mirage-unikernel-" root_pkg_name then
      let metrics_influx =
        let influx = OpamPackage.Name.of_string "metrics-influx" in
        OpamPackage.Set.exists (fun p -> OpamPackage.Name.equal p.OpamPackage.name influx)
          switch.OpamFile.SwitchExport.selections.OpamTypes.sel_installed
      in
      let mirage_monitoring =
        let monitoring = OpamPackage.Name.of_string "mirage-monitoring" in
        match OpamPackage.Name.Map.find_opt root_pkg switch.OpamFile.SwitchExport.overlays with
        | None -> false
        | Some opam ->
          let depends = OpamFile.OPAM.depends opam in
          OpamFormula.fold_left (fun acc (n', _) ->
              acc || OpamPackage.Name.equal n' monitoring)
            false depends
      in
      if metrics_influx || mirage_monitoring then
        "Unikernels (with metrics reported to Influx)"
      else
        "Unikernels"
    else
      "Packages"
  in
  match List.find_opt (fun (p, _) -> String.equal (Fpath.basename p) "opam-switch") artifacts with
  | None -> None, (None, None)
  | Some (_, data) ->
    try
      let switch = OpamFile.SwitchExport.read_from_string data in
      let root = switch.OpamFile.SwitchExport.selections.OpamTypes.sel_roots in
      assert (OpamPackage.Set.cardinal root = 1);
      let root = OpamPackage.Set.choose root in
      Some (infer_section switch root), infer_synopsis_and_descr switch root
    with _ -> None, (None, None)

let compute_input_id artifacts =
  let get_hash filename =
    match List.find_opt (fun (b, _data) -> Fpath.equal b.Builder_db.filepath filename) artifacts with
    | None -> None
    | Some (x, _data) -> Some x.sha256
  in
  match
    get_hash (Fpath.v "opam-switch"),
    get_hash (Fpath.v "build-environment"),
    get_hash (Fpath.v "system-packages")
  with
  | Some a, Some b, Some c ->
    Some Digestif.SHA256.(to_raw_string (digestv_string [a;b;c]))
  | _ -> None

let save_console_and_script staging_dir job_name uuid console script =
  let out name = Fpath.(v job_name / Uuidm.to_string uuid / name + "txt") in
  let out_staging name = Fpath.(staging_dir / name + "txt") in
  let console_to_string console =
    List.rev_map (fun (delta, data) ->
      Printf.sprintf "%.3fs:%s\n" (Duration.to_f (Int64.of_int delta)) data)
      console
    |> String.concat ""
  in
  let* () = save (out_staging "script") script in
  let* () = save (out_staging "console") (console_to_string console) in
  Ok (out "console", out "script")

let prepare_staging staging_dir =
  let* created = Bos.OS.Dir.create staging_dir in
  if not created
  then Error (`Msg "build directory already exists")
  else Ok ()

let execute_hooks ~datadir ~configdir job_name uuid platform start main_binary =
  let cachedir = cachedir datadir in
  let time =
    let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time start in
    Printf.sprintf "%04d%02d%02d%02d%02d%02d" y m d hh mm ss
  and uuid = Uuidm.to_string uuid
  and sha256 = Ohex.encode main_binary.Builder_db.sha256
  in
  let cmd cmd =
    Filename.quote_command cmd
         [ "--build-time=" ^ time ; "--sha256=" ^ sha256 ; "--job=" ^ job_name ;
           "--uuid=" ^ uuid ; "--platform=" ^ platform ;
           "--cache-dir=" ^ Fpath.to_string cachedir ;
           "--data-dir=" ^ Fpath.to_string datadir ;
           "--main-binary-filepath=" ^ Fpath.to_string main_binary.filepath ;
           Fpath.(to_string (datadir // artifact_path main_binary)) ]
  in
  Log.debug (fun m -> m "executing hooks with %s" (cmd "hook"));
  let dir = Fpath.(configdir / "upload-hooks") in
  match Unix.opendir (Fpath.to_string dir) with
  | exception Unix.Unix_error _ -> Ok ()
  | dh ->
    try
      let is_executable file =
        let st = Unix.stat (Fpath.to_string file) in
        st.Unix.st_perm land 0o111 = 0o111 &&
        st.Unix.st_kind = Unix.S_REG
      in
      let rec go () =
        let next_file = Unix.readdir dh in
        let file = Fpath.(dir / next_file) in
        (* It's important we don't execute .sh.sample files due to FreeBSD packaging *)
        if is_executable file && Fpath.has_ext ".sh" file then
          ignore (Sys.command (cmd (Fpath.to_string file) ^ " &"));
        go ()
      in
      go ()
    with
    | End_of_file ->
      Unix.closedir dh;
      Ok ()

(* saving:
   - for each artifact compute its sha256 checksum -- calling Lwt.pause in
     between
   - lookup artifact sha256 in the database and filter them out of the list: not_in_db
   - mkdir -p _staging/uuid/
   - save console & script to _staging/uuid/
   - save each artifact in not_in_db as _staging/uuid/sha256
   committing:
   - for each artifact mv _staging/uuid/sha256 _artifacts/sha256
     (or _artifacts/prefix(sha256)/sha256 where prefix(sha256) is the first two hex digits in sha256)
   - now _staging/uuid only contains console & script so we mv _staging/uuid _staging/job/uuid
   potential issues:
   - race condition in uploading same artifact:
     * if the artifact already exists in the database and thus filesystem then nothing is done
     * if the artifact is added to the database and/or filesystem we atomically overwrite it
   - input_id depends on a sort order?
   *)

let add_build ~datadir ~configdir user_id
    ((job : Builder.script_job), uuid, console, start, finish, result, raw_artifacts)
    (module Db : CONN) =
  let open Builder_db in
  let { Builder.name = job_name; platform; script } = job in
  let staging_dir = Fpath.(staging datadir / Uuidm.to_string uuid) in
  let or_cleanup r =
    Result.map_error (fun e ->
        Bos.OS.Dir.delete ~recurse:true staging_dir
        |> Result.iter_error (fun e ->
            Log.err (fun m -> m "Failed to remove staging dir %a: %a"
                        Fpath.pp staging_dir
                        pp_error e));
        e)
      r
  in
  let not_interesting p =
    String.equal (Fpath.basename p) "README.md" || String.equal (Fpath.get_ext p) ".build-hashes"
  in
  let artifacts =
    List.filter_map (fun (filepath, data) ->
        if not_interesting filepath then
          None
        else
          let sha256 = Digestif.SHA256.(to_raw_string (digest_string data))
          and size = String.length data in
          Miou.yield ();
          Some ({ filepath; sha256; size }, data))
      raw_artifacts
  in
  let* () = or_cleanup (prepare_staging staging_dir) in
  let* console, script = save_console_and_script staging_dir job_name uuid console script in
  let* artifacts_to_save =
    or_cleanup @@
    List.fold_left (fun acc (f, data) ->
        let* acc = acc in
        let* exists = Db.find Build_artifact.exists f.sha256 in
        if exists then
          Ok acc
        else
          let* () = save_artifact staging_dir f data in
          Ok (f :: acc))
      (Ok [])
      artifacts
  in
  let r =
    let* () = Db.start () in
    let* () = Db.exec Job.try_add job_name in
    let* job_id = Db.find_opt Job.get_id_by_name job_name in
    let* job_id = not_found job_id in
    let input_id = compute_input_id artifacts in
    let* () =
      Db.exec Build.add { Build.uuid; start; finish; result;
                          console; script; platform;
                          main_binary = None; input_id; user_id; job_id }
    in
    let* id = Db.find last_insert_rowid () in
    let sec_syn = infer_section_and_synopsis raw_artifacts in
    let readme =
      match List.find_opt (fun ({ filepath; _}, _) ->
          Fpath.equal filepath (Fpath.v "README.md")) artifacts
      with
      | Some (_, data) -> Some data
      | None ->
        List.find_map (fun ({ filepath; _}, data) ->
            if String.equal "README.md" (Fpath.basename filepath) then
              Some data
            else None)
          artifacts
    in
    let tag name value =
      let* () = Db.exec Tag.try_add name in
      let* tag_id = Db.find Tag.get_id_by_name name in
      let* job_tag_opt = Db.find_opt Job_tag.get_value (tag_id, job_id) in
      match job_tag_opt with
      | None ->
        Db.exec Job_tag.add (tag_id, value, job_id)
      | Some _ ->
        Db.exec Job_tag.update (tag_id, value, job_id)
    in
    let maybe_tag name opt = Option.fold ~none:(Ok ()) ~some:(tag name) opt in
    let* () = maybe_tag "readme" readme in
    let* () = maybe_tag "section" (fst sec_syn) in
    let* () = maybe_tag "synopsis" (fst (snd sec_syn)) in
    let* () = maybe_tag "description" (snd (snd sec_syn)) in
    let* main_binary, remaining_artifacts =
      match List.partition (fun (p, _data) -> Fpath.(is_prefix (v "bin/")) p.filepath) artifacts with
      | [ (main_binary, _data) ], other_artifacts ->
        let* () = Db.exec Build_artifact.add (main_binary, id) in
        let* main_binary_id = Db.find last_insert_rowid () in
        let* () = Db.exec Build.set_main_binary (id, main_binary_id) in
        Ok (Some main_binary, other_artifacts)
      | [], _ ->
        Log.debug (fun m -> m "Zero binaries for build %a" Uuidm.pp uuid);
        Ok (None, artifacts)
      | xs, _ ->
        Log.warn (fun m -> m "Multiple binaries for build %a: %a" Uuidm.pp uuid
                     Fmt.(list ~sep:(any ",") Fpath.pp)
                     (List.map (fun (a, _data) -> a.filepath) xs));
        Ok (None, artifacts)
    in
    let* () =
      List.fold_left (fun acc (file, _data) ->
          let* () = acc in
          Db.exec Build_artifact.add (file, id))
        (Ok ())
        remaining_artifacts
    in
    let* () = commit_files datadir staging_dir job_name uuid artifacts_to_save in
    let* () = Db.commit () in
    Ok main_binary
  in
  match or_cleanup r with
  | Error _ as r ->
    (match Db.rollback () with
     | Ok () -> ()
     | Error e ->
       Log.warn (fun m -> m "Failed to rollback: %a" Caqti_error.pp e));
    r
  | Ok None -> Ok ()
  | Ok Some main_binary ->
    execute_hooks job_name uuid platform start main_binary ~datadir ~configdir


(* NOTE: this function is duplicatedi in bin/builder_db_app.ml *)
let console_of_string data =
  let lines = String.split_on_char '\n' data in
  List.filter_map (fun line ->
      match String.index line ':' with
      | 0 -> Log.warn (fun m -> m "console line starting with colon %S" line); None
      | i ->
        (* the timestamp is of the form "%fs", e.g. 0.867s; so chop off the 's' *)
        let delta = float_of_string (String.sub line 0 (i - 1)) in
        let delta = Int64.to_int (Duration.of_f delta) in
        let line = String.sub line (succ i) (String.length line - (succ i)) in
        Some (delta, line)
      | exception Not_found ->
        if line <> "" then
          Log.warn (fun m -> m "Unexpected console line %S" line);
        None)
    lines
  |> List.rev (* in the exec format the order is reversed *)

let exec_of_build datadir uuid (module Db : CONN) =
  let open Builder_db in
  let* r = Db.find_opt Build.get_by_uuid uuid in
  let* (build_id, build) = not_found r in
  let { Build.start; finish; result; job_id; console; script; platform; _ } = build in
  let* job_name = Db.find Job.get job_id in
  let* script = read_file datadir script in
  let* console = read_file datadir console in
  let job = { Builder.name = job_name; script; platform } in
  let out = console_of_string console in
  let* artifacts = Db.collect_list Build_artifact.get_all_by_build build_id in
  let* readme_opt = readme job_id (module Db) in
  let* data =
    List.fold_left
      (fun acc (_id, ({ filepath; _ } as file)) ->
         let* acc = acc in
         let* data = read_file datadir (artifact_path file) in
         Ok ((filepath, data) :: acc))
      (Ok []) artifacts
  in
  let data = Option.fold ~none:data ~some:(fun readme -> (Fpath.v "README.md", readme) :: data) readme_opt in
  let exec = (job, uuid, out, start, finish, result, data) in
  Ok (Builder.Asn.exec_to_str exec)

module Viz = struct
  open Viz

  let hash_viz_input ~uuid typ conn =
    let open Builder_db in
    match typ with
    | `Treemap ->
      let* (_build_id, build) = build uuid conn in
      let* main_binary = not_found build.main_binary in
      let* main_binary = build_artifact_by_id main_binary conn in
      let* debug_binary =
        let bin = Fpath.(base main_binary.filepath + "debug") in
        build_artifact uuid bin conn
      in
      Ok (Ohex.encode debug_binary.sha256)
    | `Dependencies ->
      let* opam_switch = build_artifact uuid (Fpath.v "opam-switch") conn in
      Ok (Ohex.encode opam_switch.sha256)

  let try_load_cached_visualization ~datadir ~uuid viz_typ conn =
    let cachedir = cachedir datadir in
    let* latest_viz_version = get_viz_version_from_dirs ~cachedir ~viz_typ in
    let* viz_input_hash = hash_viz_input ~uuid viz_typ conn in
    choose_versioned_viz_path
      ~cachedir
      ~current_version:latest_viz_version
      ~viz_typ
      ~viz_input_hash

end

open Lwt_result.Infix

let src = Logs.Src.create "builder-web.model" ~doc:"Builder_web model"
module Log = (val Logs.src_log  src : Logs.LOG)

module type CONN = Caqti_lwt.CONNECTION

type error = [ Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t | `Msg of string ]

let pp_error ppf = function
  | `Not_found -> Format.fprintf ppf "value not found in database"
  | `File_error path -> Format.fprintf ppf "error reading file %a" Fpath.pp path
  | `Msg e -> Format.fprintf ppf "error %s" e
  | #Caqti_error.call_or_retrieve as e ->
    Caqti_error.pp ppf e

let not_found = function
  | None -> Lwt_result.fail `Not_found
  | Some v -> Lwt_result.return v

let staging datadir = Fpath.(datadir / "_staging")
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
  Lwt.try_bind
    (fun () -> Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string filepath))
    (fun ic ->
       let open Lwt.Infix in
       Lwt_io.read ic >>= fun data ->
       Lwt_io.close ic >>= fun () ->
       Lwt_result.return data)
    (function
      | Unix.Unix_error (e, _, _) ->
        Log.warn (fun m -> m "Error reading local file %a: %s"
                      Fpath.pp filepath (Unix.error_message e));
        Lwt.return_error (`File_error filepath)
      | e -> Lwt.reraise e)

let build_artifact build filepath (module Db : CONN) =
  Db.find_opt Builder_db.Build_artifact.get_by_build_uuid (build, filepath)
  >>= not_found >|= snd

let build_artifact_by_id id (module Db : CONN) =
  Db.find Builder_db.Build_artifact.get id

let build_artifact_data datadir file =
  read_file datadir (artifact_path file)

let build_artifact_stream_data datadir file =
  let open Lwt.Syntax in
  let filepath = Fpath.(datadir // artifact_path file) in
  Lwt.catch
    (fun () -> Lwt_result.ok (Lwt_unix.openfile (Fpath.to_string filepath) Unix.[ O_RDONLY ] 0))
    (function
      | Unix.Unix_error (e, _, _) ->
        Log.warn (fun m -> m "Error reading local file %a: %s"
                      Fpath.pp filepath (Unix.error_message e));
        Lwt.return_error (`File_error filepath)
      | e -> Lwt.reraise e)
  >|= fun fd ->
  fun ~write ~close ->
    Lwt.finalize
      (fun () ->
         let buf = Bytes.create 65536 in
         Bytes.fill buf 0 (Bytes.length buf) '\000';
         let rec loop () =
           (* XXX(reynir): we don't handle Unix_error exceptions here *)
           let* l = Lwt_unix.read fd buf 0 (Bytes.length buf) in
           if l > 0 then
             let* () =
               let s =
                 if l = Bytes.length buf then
                   Bytes.unsafe_to_string buf
                 else Bytes.sub_string buf 0 l in
               write s
             in
             loop ()
           else
             close ()
         in
         loop ())
      (fun () ->
         Lwt.catch (fun () -> Lwt_unix.close fd)
           (function
             | Unix.Unix_error _ -> Lwt.return_unit
             | e -> Lwt.reraise e))

let build_artifacts build (module Db : CONN) =
  Db.collect_list Builder_db.Build_artifact.get_all_by_build build >|=
  List.map snd

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

let platforms_of_job id (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_platforms_for_job id

let build uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>=
  not_found

let build_with_main_binary job platform (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest_successful_with_binary (job, platform) >|=
  Option.map (fun (_id, build, file) -> (build, file))

let build_hash hash (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_with_jobname_by_hash hash

let build_exists uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >|=
  Option.is_some

let latest_successful_build job_id platform (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest_successful (job_id, platform)

let latest_successful_build_uuid job_id platform db =
  latest_successful_build job_id platform db >|= fun build ->
  Option.map (fun build -> build.Builder_db.Build.uuid) build

let previous_successful_build_different_output id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_previous_successful_different_output id

let next_successful_build_different_output id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_next_successful_different_output id

let failed_builds ~start ~count platform (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_all_failed (start, count, platform)

let builds_with_different_input_and_same_main_binary id (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_different_input_same_output_input_ids id >>= fun ids ->
  Lwt_list.fold_left_s (fun acc input_id ->
     match acc with
     | Error _ as e -> Lwt.return e
     | Ok metas ->
       Db.find Builder_db.Build.get_one_by_input_id input_id >>= fun build ->
       Lwt.return (Ok (build :: metas)))
   (Ok []) ids

let builds_with_same_input_and_same_main_binary id (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_same_input_same_output_builds id

let builds_with_same_input_and_different_main_binary id (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_same_input_different_output_hashes id >>= fun hashes ->
  Lwt_list.fold_left_s (fun acc hash ->
     match acc with
     | Error _ as e -> Lwt.return e
     | Ok metas ->
       Db.find Builder_db.Build.get_by_hash hash >>= fun build ->
       Lwt.return (Ok (build :: metas)))
   (Ok []) hashes

let build_console_by_uuid datadir uuid (module Db : CONN) =
  build uuid (module Db) >>= fun (_id, { Builder_db.Build.console; _ })->
  read_file datadir console

let build_script_by_uuid datadir uuid (module Db : CONN) =
  build uuid (module Db) >>= fun (_id, { Builder_db.Build.script; _ })->
  read_file datadir script

let job_id job_name (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name

let readme job (module Db : CONN) =
  job_id job (module Db) >>= not_found >>= fun job_id ->
  Db.find Builder_db.Tag.get_id_by_name "readme.md" >>= fun readme_id ->
  Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id)

let job_and_readme job (module Db : CONN) =
  job_id job (module Db) >>= not_found >>= fun job_id ->
  Db.find Builder_db.Tag.get_id_by_name "readme.md" >>= fun readme_id ->
  Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id) >|= fun readme ->
  job_id, readme

let builds_grouped_by_output job_id platform (module Db : CONN) =
  Db.collect_list Builder_db.Build.get_all_artifact_sha (job_id, platform) >>= fun sha ->
  Lwt_list.fold_left_s (fun acc hash ->
    match acc with
    | Error _ as e -> Lwt.return e
    | Ok builds ->
      Db.find Builder_db.Build.get_with_main_binary_by_hash hash >|= fun b ->
      b :: builds)
    (Ok []) sha >|= List.rev

let builds_grouped_by_output_with_failed job_id platform ((module Db : CONN) as db) =
  builds_grouped_by_output job_id platform db >>= fun builds ->
  Db.collect_list Builder_db.Build.get_failed_builds (job_id, platform) >|= fun failed ->
  let failed = List.map (fun b -> b, None) failed in
  let cmp (a, _) (b, _) = Ptime.compare b.Builder_db.Build.start a.Builder_db.Build.start in
  List.merge cmp builds failed

let jobs_with_section_synopsis (module Db : CONN) =
  Db.collect_list Builder_db.Job.get_all_with_section_synopsis ()

let job_name id (module Db : CONN) =
  Db.find Builder_db.Job.get id

let user username (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username

let authorized user_id job_name (module Db : CONN) =
  job_id job_name (module Db) >>=
  (function None -> Lwt_result.fail (`Msg "No such job") | Some r -> Lwt_result.return r) >>= fun job_id ->
  Db.find Builder_db.Access_list.get (user_id, job_id) >|= fun _id ->
  ()

let cleanup_staging datadir (module Db : Caqti_lwt.CONNECTION) =
  let cleanup_staged staged =
    match Uuidm.of_string (Fpath.to_string staged) with
    | None ->
      Log.warn (fun m -> m "Non-uuid staged files: %a" Fpath.pp
                   Fpath.(staging datadir // staged));
      Lwt.return (Bos.OS.Path.delete ~recurse:true Fpath.(staging datadir // staged))
    | Some uuid ->
      let staged = Fpath.(staging datadir // staged) in
      Db.find_opt Builder_db.Build.get_by_uuid uuid >>= function
      | Some (_id, build) ->
        Db.find Builder_db.Job.get build.job_id >>= fun job_name ->
        let destdir = Fpath.(datadir / job_name / Uuidm.to_string uuid) in
        Lwt.return (Bos.OS.Path.move staged destdir)
      | None ->
        Lwt.return (Bos.OS.Path.delete ~recurse:true Fpath.(staging datadir // staged))
  in
  Lwt.return (Bos.OS.Dir.contents ~rel:true (staging datadir)) >>= fun stageds ->
  Lwt_result.ok @@
  Lwt_list.iter_s
    (fun staged ->
       Lwt.map (function
       | Error (_ as e) ->
         Log.warn (fun m -> m "Failed cleaning up staged files %a in %a: %a"
                      Fpath.pp staged
                      Fpath.pp Fpath.(staging datadir // staged)
                      pp_error e)
       | Ok () -> ())
       (cleanup_staged staged))
    stageds

let save path data =
  let open Lwt.Infix in
  Lwt.catch
    (fun () ->
       Lwt_io.open_file ~mode:Lwt_io.Output (Fpath.to_string path) >>= fun oc ->
       Lwt_io.write oc data >>= fun () ->
       Lwt_io.close oc
       |> Lwt_result.ok)
    (function
      | Unix.Unix_error (e, _, _) ->
          Lwt_result.fail (`Msg (Unix.error_message e))
      | e -> Lwt.reraise e)

let save_artifacts staging artifacts =
  List.fold_left
    (fun r (file, data) ->
       r >>= fun () ->
       let sha256 = Ohex.encode file.Builder_db.sha256 in
       let destpath = Fpath.(staging / sha256) in
       save destpath data)
    (Lwt_result.return ())
    artifacts

let commit_files datadir staging_dir job_name uuid artifacts =
  (* First we move the artifacts *)
  List.fold_left
    (fun r artifact ->
       r >>= fun () ->
       let sha256 = Ohex.encode artifact.Builder_db.sha256 in
       let src = Fpath.(staging_dir / sha256) in
       let dest = Fpath.(datadir // artifact_path artifact) in
       Lwt.return (Bos.OS.Dir.create (Fpath.parent dest)) >>= fun _created ->
       Lwt.return (Bos.OS.Path.move ~force:true src dest))
    (Lwt_result.return ())
    artifacts >>= fun () ->
  (* Now the staging dir only contains script & console *)
  let job_dir = Fpath.(datadir / job_name) in
  let dest = Fpath.(job_dir / Uuidm.to_string uuid) in
  Lwt.return (Bos.OS.Dir.create job_dir) >>= fun _ ->
  Lwt.return (Bos.OS.Path.move staging_dir dest)

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
    match List.find_opt (fun b -> Fpath.equal b.Builder_db.filepath filename) artifacts with
    | None -> None
    | Some x -> Some x.sha256
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
  save (out_staging "script") script >>= fun () ->
  save (out_staging "console") (console_to_string console) >|= fun () ->
  (out "console", out "script")

let prepare_staging staging_dir =
  Lwt.return (Bos.OS.Dir.create staging_dir) >>= fun created ->
  if not created
  then Lwt_result.fail (`Msg "build directory already exists")
  else Lwt_result.return ()

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


let add_build
    ~datadir
    ~cachedir
    ~configdir
    user_id
    ((job : Builder.script_job), uuid, console, start, finish, result, raw_artifacts)
    (module Db : CONN) =
  let open Builder_db in
  let job_name = job.Builder.name in
  let staging_dir = Fpath.(staging datadir / Uuidm.to_string uuid) in
  let or_cleanup x =
    Lwt_result.map_error (fun e ->
        Bos.OS.Dir.delete ~recurse:true staging_dir
        |> Result.iter_error (fun e ->
            Log.err (fun m -> m "Failed to remove staging dir %a: %a"
                        Fpath.pp staging_dir
                        pp_error e));
        e)
      x
  in
  let not_interesting p =
    String.equal (Fpath.basename p) "README.md" || String.equal (Fpath.get_ext p) ".build-hashes"
  in
  begin
    List.fold_left
      (fun r (filepath, data) ->
         r >>= fun acc ->
         if not_interesting filepath then
           Lwt_result.return acc
         else
           let sha256 = Digestif.SHA256.(to_raw_string (digest_string data))
           and size = String.length data in
           Lwt_result.ok (Lwt.pause ()) >|= fun () ->
           ({ filepath; sha256; size }, data) :: acc)
      (Lwt_result.return [])
      raw_artifacts
  end >>= fun artifacts ->
  or_cleanup (prepare_staging staging_dir) >>= fun () ->
  or_cleanup (save_console_and_script staging_dir job_name uuid console job.Builder.script)
  >>= fun (console, script) ->
  List.fold_left
    (fun r ((f, _) as artifact) ->
       r >>= fun acc ->
       Db.find Builder_db.Build_artifact.exists f.sha256 >|= fun exists ->
       if exists then acc else artifact :: acc)
    (Lwt_result.return [])
    artifacts >>= fun artifacts_to_save ->
  or_cleanup (save_artifacts staging_dir artifacts_to_save) >>= fun () ->
  let artifacts = List.map fst artifacts in
  let r =
    Db.start () >>= fun () ->
    Db.exec Job.try_add job_name >>= fun () ->
    Db.find_opt Job.get_id_by_name job_name >>= fun job_id ->
    Lwt.return (Option.to_result ~none:(`Msg "No such job id") job_id) >>= fun job_id ->
    let section_tag = "section" in
    Db.exec Tag.try_add section_tag >>= fun () ->
    Db.find Tag.get_id_by_name section_tag >>= fun section_id ->
    let synopsis_tag = "synopsis" in
    Db.exec Tag.try_add synopsis_tag >>= fun () ->
    Db.find Tag.get_id_by_name synopsis_tag >>= fun synopsis_id ->
    let descr_tag = "description" in
    Db.exec Tag.try_add descr_tag >>= fun () ->
    Db.find Tag.get_id_by_name descr_tag >>= fun descr_id ->
    let readme_tag = "readme.md" in
    Db.exec Tag.try_add readme_tag >>= fun () ->
    Db.find Tag.get_id_by_name readme_tag >>= fun readme_id ->
    let input_id = compute_input_id artifacts in
    let platform = job.Builder.platform in
    Db.exec Build.add { Build.uuid; start; finish; result;
                        console; script; platform;
                        main_binary = None; input_id; user_id; job_id } >>= fun () ->
    Db.find last_insert_rowid () >>= fun id ->
    let sec_syn = infer_section_and_synopsis raw_artifacts in
    let add_or_update tag_id tag_value =
      Db.find_opt Job_tag.get_value (tag_id, job_id) >>= function
      | None -> Db.exec Job_tag.add (tag_id, tag_value, job_id)
      | Some _ -> Db.exec Job_tag.update (tag_id, tag_value, job_id)
    in
    (match fst sec_syn with
     | None -> Lwt_result.return ()
     | Some section_v -> add_or_update section_id section_v) >>= fun () ->
    (match snd sec_syn with
     | None, _-> Lwt_result.return ()
     | Some synopsis_v, _ -> add_or_update synopsis_id synopsis_v) >>= fun () ->
    (match snd sec_syn with
     | _, None -> Lwt_result.return ()
     | _, Some descr_v -> add_or_update descr_id descr_v) >>= fun () ->
    (let readme =
       List.find_opt (fun (p, _) -> Fpath.(equal (v "README.md") p)) raw_artifacts
     in
     let readme_anywhere =
       List.find_opt (fun (p, _) -> String.equal "README.md" (Fpath.basename p)) raw_artifacts
     in
     match readme, readme_anywhere with
     | None, None -> Lwt_result.return ()
     | Some (_, data), _ | None, Some (_, data) -> add_or_update readme_id data) >>= fun () ->
    (match List.partition (fun p -> Fpath.(is_prefix (v "bin/") p.filepath)) artifacts with
     | [ main_binary ], other_artifacts ->
       Db.exec Build_artifact.add (main_binary, id) >>= fun () ->
       Db.find Builder_db.last_insert_rowid () >>= fun main_binary_id ->
       Db.exec Build.set_main_binary (id, main_binary_id) >|= fun () ->
       Some main_binary, other_artifacts
     | [], _ ->
       Log.debug (fun m -> m "Zero binaries for build %a" Uuidm.pp uuid);
       Lwt_result.return (None, artifacts)
     | xs, _ ->
       Log.warn (fun m -> m "Multiple binaries for build %a: %a" Uuidm.pp uuid
                    Fmt.(list ~sep:(any ",") Fpath.pp)
                    (List.map (fun a -> a.filepath) xs));
       Lwt_result.return (None, artifacts)) >>= fun (main_binary, remaining_artifacts_to_add) ->
    List.fold_left
      (fun r file ->
         r >>= fun () ->
         Db.exec Build_artifact.add (file, id))
      (Lwt_result.return ())
      remaining_artifacts_to_add >>= fun () ->
    commit_files datadir staging_dir job_name uuid (List.map fst artifacts_to_save) >>= fun () ->
    Db.commit () >|= fun () ->
    main_binary
  in
  Lwt_result.bind_lwt_error (or_cleanup r)
    (fun e ->
       Db.rollback ()
       |> Lwt.map (fun r ->
           Result.iter_error
             (fun e' -> Log.err (fun m -> m "Failed rollback: %a" Caqti_error.pp e'))
             r;
           e)) >>= function
  | None -> Lwt.return (Ok ())
  | Some main_binary ->
    let time =
      let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time start in
      Printf.sprintf "%04d%02d%02d%02d%02d%02d" y m d hh mm ss
    and uuid = Uuidm.to_string uuid
    and job = job.name
    and platform = job.platform
    and sha256 = Ohex.encode main_binary.sha256
    in
    let fp_str p = Fpath.(to_string (datadir // p)) in
    let args =
      String.concat " "
        (List.map (fun s -> "\"" ^ String.escaped s ^ "\"")
           [ "--build-time=" ^ time ; "--sha256=" ^ sha256 ; "--job=" ^ job ;
             "--uuid=" ^ uuid ; "--platform=" ^ platform ;
             "--cache-dir=" ^ Fpath.to_string cachedir ;
             "--data-dir=" ^ Fpath.to_string datadir ;
             "--main-binary-filepath=" ^ Fpath.to_string main_binary.filepath ;
             fp_str Fpath.(datadir // artifact_path main_binary) ])
    in
    Log.debug (fun m -> m "executing hooks with %s" args);
    let dir = Fpath.(configdir / "upload-hooks") in
    (try
       Lwt.return (Ok (Some (Unix.opendir (Fpath.to_string dir))))
     with
       Unix.Unix_error _ -> Lwt.return (Ok None)) >>= function
    | None -> Lwt.return (Ok ())
    | Some dh ->
      try
        let is_executable file =
          let st = Unix.stat (Fpath.to_string file) in
          st.Unix.st_perm land 0o111 = 0o111 &&
          st.Unix.st_kind = Unix.S_REG
        in
        let rec go () =
          let next_file = Unix.readdir dh in
          let file = Fpath.(dir / next_file) in
          if is_executable file && Fpath.has_ext ".sh" file then
            ignore (Sys.command (Fpath.to_string file ^ " " ^ args ^ " &"));
          go ()
        in
        go ()
      with
      | End_of_file ->
        Unix.closedir dh;
        Lwt.return (Ok ())

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
        let line = String.sub line i (String.length line - i) in
        Some (delta, line)
      | exception Not_found ->
        if line <> "" then
          Log.warn (fun m -> m "Unexpected console line %S" line);
        None)
    lines

let exec_of_build datadir uuid (module Db : CONN) =
  let open Builder_db in
  Db.find_opt Build.get_by_uuid uuid >>= not_found >>= fun (build_id, build) ->
  let { Builder_db.Build.start; finish; result;
        job_id; console; script; platform; _ } =
    build
  in
  Db.find Builder_db.Job.get job_id >>= fun job_name ->
  read_file datadir script >>= fun script ->
  let job = { Builder.name = job_name; platform; script } in
  read_file datadir console >>= fun console ->
  let out = console_of_string console in
  Db.collect_list Builder_db.Build_artifact.get_all_by_build build_id >>= fun artifacts ->
  readme job_name (module Db) >>= fun readme_opt ->
  Lwt_list.fold_left_s (fun acc (_id, ({ filepath; _ } as file)) ->
      match acc with
      | Error _ as e -> Lwt.return e
      | Ok acc ->
        build_artifact_data datadir file >>= fun data ->
        Lwt.return (Ok ((filepath, data) :: acc)))
    (Ok []) artifacts >>= fun data ->
  let data =
    match readme_opt with
    | None -> data
    | Some readme -> (Fpath.v "README.md", readme) :: data
  in
  let exec = (job, uuid, out, start, finish, result, data) in
  let data = Builder.Asn.exec_to_str exec in
  Lwt.return (Ok data)

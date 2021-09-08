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
  | None -> Lwt.return (Error `Not_found :> (_, [> error ]) result)
  | Some v -> Lwt_result.return v

let staging datadir = Fpath.(datadir / "_staging")

let read_file datadir filepath =
  let filepath = Fpath.(datadir // filepath) in
  Lwt.try_bind
    (fun () -> Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string filepath))
    (fun ic -> Lwt_result.ok (Lwt_io.read ic))
    (function
      | Unix.Unix_error (e, _, _) ->
        Logs.warn (fun m -> m "Error reading local file %a: %s"
                      Fpath.pp filepath (Unix.error_message e));
        Lwt.return_error (`File_error filepath)
      | e -> Lwt.fail e)

let build_artifact build filepath (module Db : CONN) =
  Db.find_opt Builder_db.Build_artifact.get_by_build_uuid (build, filepath)
  >>= not_found >|= snd

let build_artifact_by_id id (module Db : CONN) =
  Db.find Builder_db.Build_artifact.get id

let build_artifact_data datadir file =
  read_file datadir file.Builder_db.localpath

let build_artifacts build (module Db : CONN) =
  Db.collect_list Builder_db.Build_artifact.get_all_by_build build >|=
  List.map snd

let build uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>=
  not_found

let build_meta job (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest job >|=
  Option.map (fun (_id, meta, file) -> (meta, file))

let build_hash hash (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_with_jobname_by_hash hash

let build_exists uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >|=
  Option.is_some

let latest_build_uuid job_id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest_uuid job_id >>=
  (* We know there's at least one job when this is called, probably. *)
  not_found >|= snd

let latest_successful_build_uuid job_id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_latest_successful_uuid job_id

let previous_successful_build id (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_previous_successful id >|=
  Option.map (fun (_id, meta) -> meta)

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

let job_id job_name (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name

let readme job (module Db : CONN) =
  job_id job (module Db) >>= not_found >>= fun job_id ->
  Db.find Builder_db.Tag.get_id_by_name "readme.md" >>= fun readme_id ->
  Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id)

let job_and_readme job (module Db : CONN) =
  job_id job (module Db) >>= not_found >>= fun job_id ->
  Db.find Builder_db.Tag.get_id_by_name "readme.md" >>= fun readme_id ->
  Db.find_opt Builder_db.Job_tag.get_value (readme_id, job_id) >>= fun readme ->
  Db.find_opt Builder_db.Build.get_latest_failed job_id >>= fun failed ->
  Db.collect_list Builder_db.Build.get_all_artifact_sha job_id >>= fun sha ->
  Lwt_list.fold_left_s (fun acc hash ->
     match acc with
     | Error _ as e -> Lwt.return e
     | Ok (fail, builds) ->
       Db.find Builder_db.Build.get_with_main_binary_by_hash hash >|= fun (build, file) ->
       match fail with
       | Some f when Ptime.is_later ~than:build.Builder_db.Build.start f.Builder_db.Build.start -> None, (build, file) :: (f, None) :: builds
       | x -> x, (build, file) :: builds)
    (Ok (failed, [])) sha >|= fun (x, builds) ->
  let builds = match x with None -> builds | Some f -> (f, None) :: builds in
  readme, List.rev builds

let jobs (module Db : CONN) =
  Db.collect_list Builder_db.Job.get_all ()

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

let save file data =
  let open Lwt.Infix in
  Lwt.catch
    (fun () ->
       Lwt_io.open_file ~mode:Lwt_io.Output (Fpath.to_string file) >>= fun oc ->
       Lwt_io.write oc data >>= fun () ->
       Lwt_io.close oc
       |> Lwt_result.ok)
    (function
      | Unix.Unix_error (e, _, _) ->
          Lwt_result.fail (`Msg (Unix.error_message e))
      | e -> Lwt.fail e)

let save_file dir staging (filepath, data) =
  let size = String.length data in
  let sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
  let localpath = Fpath.append dir filepath in
  let destpath = Fpath.append staging filepath in
  Lwt_result.lift (Bos.OS.Dir.create (Fpath.parent destpath)) >>= fun _ ->
  save destpath data >|= fun () ->
  { Builder_db.filepath; localpath; sha256; size }

let save_files dir staging files =
  List.fold_left
    (fun r file ->
       r >>= fun acc ->
       save_file dir staging file >>= fun file ->
       Lwt_result.return (file :: acc))
    (Lwt_result.return [])
    files

let save_all staging_dir (job : Builder.script_job) uuid artifacts =
  let build_dir = Fpath.(v job.Builder.name / Uuidm.to_string uuid) in
  let output_dir = Fpath.(build_dir / "output")
  and staging_output_dir = Fpath.(staging_dir / "output") in
  Lwt.return (Bos.OS.Dir.create staging_output_dir) >>= fun _ ->
  save_files output_dir staging_output_dir artifacts >>= fun artifacts ->
  Lwt_result.return artifacts

let commit_files datadir staging_dir job_name uuid =
  let job_dir = Fpath.(datadir / job_name) in
  let dest = Fpath.(job_dir / Uuidm.to_string uuid) in
  Lwt.return (Bos.OS.Dir.create job_dir) >>= fun _ ->
  Lwt.return (Bos.OS.Path.move staging_dir dest)

let infer_section_and_synopsis name artifacts =
  let opam_switch =
    match List.find_opt (fun (p, _) -> String.equal (Fpath.basename p) "opam-switch") artifacts with
    | None -> None
    | Some (_, data) -> Some (OpamFile.SwitchExport.read_from_string data)
  in
  let infer_synopsis_and_descr switch =
    let root = switch.OpamFile.SwitchExport.selections.OpamTypes.sel_roots in
    if OpamPackage.Set.cardinal root <> 1 then
      None, None
    else
      let root = OpamPackage.Set.choose root in
      match OpamPackage.Name.Map.find_opt root.OpamPackage.name switch.OpamFile.SwitchExport.overlays with
      | None -> None, None
      | Some opam -> OpamFile.OPAM.synopsis opam, OpamFile.OPAM.descr_body opam
  in
  let infer_section_from_packages switch =
    let influx = OpamPackage.Name.of_string "metrics-influx" in
    if OpamPackage.Set.exists (fun p -> OpamPackage.Name.equal p.OpamPackage.name influx)
         switch.OpamFile.SwitchExport.selections.OpamTypes.sel_installed
    then
      "Unikernels (with metrics reported to Influx)"
    else
      "Unikernels"
  in
  let infer_section_from_name name =
    let map = [
      "-freebsd", "FreeBSD" ;
      "-debian", "Debian" ;
      "-ubuntu", "Ubuntu" ;
    ] in
    match
      List.find_opt (fun (affix, _) -> Astring.String.is_infix ~affix name) map
    with
    | None -> None
    | Some (_, os) -> Some (os ^ " Packages")
  in
  match opam_switch with
  | None -> None, (None, None)
  | Some opam_switch ->
    let section =
      match infer_section_from_name name with
      | Some x -> x
      | None -> infer_section_from_packages opam_switch
    in
    Some section, infer_synopsis_and_descr opam_switch

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
  | Some a, Some b, Some c -> Some (Mirage_crypto.Hash.SHA256.digest (Cstruct.concat [a;b;c]))
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
  (out "script", out "console")

let prepare_staging staging_dir =
  Lwt.return (Bos.OS.Dir.create staging_dir) >>= fun created ->
  if not created
  then Lwt_result.fail (`Msg "build directory already exists")
  else Lwt_result.return ()

let add_build
    datadir
    user_id
    ((job : Builder.script_job), uuid, console, start, finish, result, raw_artifacts)
    (module Db : CONN) =
  let open Builder_db in
  let job_name = job.Builder.name in
  let staging_dir = Fpath.(staging datadir / Uuidm.to_string uuid) in
  let or_cleanup x =
    Lwt_result.map_err (fun e ->
        Bos.OS.Dir.delete ~recurse:true staging_dir
        |> Result.iter_error (fun e ->
            Log.err (fun m -> m "Failed to remove staging dir %a: %a"
                        Fpath.pp staging_dir
                        pp_error e));
        e)
      x
  in
  let artifacts_to_preserve =
    let not_interesting p =
      String.equal (Fpath.basename p) "README.md" || String.equal (Fpath.get_ext p) ".build-hashes"
    in
    List.filter (fun (p, _) -> not (not_interesting p)) raw_artifacts
  in
  or_cleanup (prepare_staging staging_dir) >>= fun () ->
  or_cleanup (save_console_and_script staging_dir job_name uuid console job.Builder.script)
  >>= fun (console, script) ->
  or_cleanup (save_all staging_dir job uuid artifacts_to_preserve) >>= fun artifacts ->
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
    Db.exec Build.add { Build.uuid; start; finish; result;
                        console; script;
                        main_binary = None; input_id; user_id; job_id } >>= fun () ->
    Db.find last_insert_rowid () >>= fun id ->
    let sec_syn = infer_section_and_synopsis job_name raw_artifacts in
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
    List.fold_left
      (fun r file ->
         r >>= fun () ->
         Db.exec Build_artifact.add (file, id))
      (Lwt_result.return ())
      artifacts >>= fun () ->
    Db.collect_list Build_artifact.get_all_by_build id >>= fun artifacts ->
    (match List.filter (fun (_, p) -> Fpath.(is_prefix (v "bin/") p.filepath)) artifacts with
     | [ (build_artifact_id, _) ] -> Db.exec Build.set_main_binary (id, build_artifact_id)
     | [] ->
       Log.debug (fun m -> m "Zero binaries for build %a" Uuidm.pp uuid);
       Lwt_result.return ()
     | xs ->
       Log.warn (fun m -> m "Multiple binaries for build %a: %a" Uuidm.pp uuid
                    Fmt.(list ~sep:(any ",") Fpath.pp)
                    (List.map (fun (_, a) -> a.filepath) xs));
       Lwt_result.return ()) >>= fun () ->
    Db.commit () >>= fun () ->
    commit_files datadir staging_dir job_name uuid
  in
  Lwt_result.bind_lwt_err (or_cleanup r)
    (fun e ->
       Db.rollback ()
       |> Lwt.map (fun r ->
           Result.iter_error
             (fun e' -> Log.err (fun m -> m "Failed rollback: %a" Caqti_error.pp e'))
             r;
           e))

open Caqti_request.Infix

let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let or_die exit_code = function
  | Ok r -> r
  | Error (`Msg msg) ->
    Format.eprintf "Error: %s\n" msg;
    exit exit_code
  | Error (#Caqti_error.t as e) ->
    Format.eprintf "Database error: %a\n" Caqti_error.pp e;
    exit exit_code

let defer_foreign_keys =
  Caqti_type.unit ->. Caqti_type.unit @@
  "PRAGMA defer_foreign_keys = ON"

let connect uri =
  let* (module Db : Caqti_blocking.CONNECTION) = Caqti_blocking.connect ~tweaks_version:(1,8) uri in
  let* () = Db.exec defer_foreign_keys () in
  Ok (module Db : Caqti_blocking.CONNECTION)

let do_migrate dbpath =
  let* (module Db : Caqti_blocking.CONNECTION) =
    connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ())
  in
  List.fold_left
    (fun r migrate ->
       let* () = r in
       Logs.debug (fun m -> m "Executing migration query: %a" Caqti_request.pp migrate);
       Db.exec migrate ())
    (Ok ())
    Builder_db.migrate

let migrate () dbpath =
  or_die 1 (do_migrate dbpath)

let user_mod action dbpath scrypt_n scrypt_r scrypt_p username unrestricted =
  let scrypt_params = Builder_web_auth.scrypt_params ?scrypt_n ?scrypt_r ?scrypt_p () in
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    print_string "Password: ";
    flush stdout;
    (* FIXME: getpass *)
    let password = read_line () in
    let restricted = not unrestricted in
    let user_info = Builder_web_auth.hash ~scrypt_params ~username ~password ~restricted () in
    match action with
    | `Add ->
      Db.exec Builder_db.User.add user_info
    | `Update ->
      Db.exec Builder_db.User.update_user user_info
  in
  or_die 1 r

let user_add () dbpath = user_mod `Add dbpath

let user_update () dbpath = user_mod `Update dbpath

let user_list () dbpath =
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    Db.iter_s Builder_db.User.get_all
      (fun username -> Ok (print_endline username))
      ()
  in
  or_die 1 r

let user_remove () dbpath username =
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* () = Db.exec Builder_db.Access_list.remove_all_by_username username in
    Db.exec Builder_db.User.remove_user username
  in
  or_die 1 r

let user_disable () dbpath username =
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* () = Db.exec Builder_db.Access_list.remove_all_by_username username in
    let* user = Db.find_opt Builder_db.User.get_user username in
    match user with
    | None -> Error (`Msg "user not found")
    | Some (_, user_info) ->
      let password_hash = `Scrypt (Cstruct.empty, Cstruct.empty, Builder_web_auth.scrypt_params ()) in
      let user_info = { user_info with password_hash ; restricted = true } in
      Db.exec Builder_db.User.update_user user_info
  in
  or_die 1 r

let access_add () dbpath username jobname =
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* (user_id, _) =
      Result.bind (Db.find_opt Builder_db.User.get_user username)
        (Option.to_result ~none:(`Msg "unknown user"))
    in
    let* job_id =
      Result.bind (Db.find_opt Builder_db.Job.get_id_by_name jobname)
        (Option.to_result ~none:(`Msg "job not found"))
    in
    Db.exec Builder_db.Access_list.add (user_id, job_id)
   in
   or_die 1 r

let access_remove () dbpath username jobname =
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* (user_id, _) =
      Result.bind (Db.find_opt Builder_db.User.get_user username)
        (Option.to_result ~none:(`Msg "unknown user"))
    in
    let* (job_id) =
      Result.bind (Db.find_opt Builder_db.Job.get_id_by_name jobname)
        (Option.to_result ~none:(`Msg "job not found"))
    in
    Db.exec Builder_db.Access_list.remove (user_id, job_id)
   in
   or_die 1 r

let job_remove () datadir jobname =
  let dbpath = datadir ^ "/builder.sqlite3" in
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* job_id_opt = Db.find_opt Builder_db.Job.get_id_by_name jobname in
    match job_id_opt with
    | None ->
      Logs.info (fun m -> m "Job %S doesn't exist or has already been removed." jobname);
      Ok ()
    | Some job_id ->
      let* () = Db.start () in
      let* () = Db.exec defer_foreign_keys () in
      let r =
        let* builds = Db.collect_list Builder_db.Build.get_all job_id in
        let* () =
          List.fold_left (fun r (build_id, build) ->
              let* () = r in
              let dir = Fpath.(v datadir / jobname / Uuidm.to_string build.Builder_db.Build.uuid) in
              (match Bos.OS.Dir.delete ~recurse:true dir with
               | Ok _ -> ()
               | Error `Msg e -> Logs.warn (fun m -> m "failed to remove build directory %a: %s" Fpath.pp dir e));
              let* () = Db.exec Builder_db.Build_artifact.remove_by_build build_id in
              Db.exec Builder_db.Build.remove build_id)
            (Ok ())
            builds
        in
        let* () = Db.exec Builder_db.Access_list.remove_by_job job_id in
        let* () = Db.exec Builder_db.Job_tag.remove_by_job job_id in
        let* () = Db.exec Builder_db.Job.remove job_id in
        Db.commit ()
      in
      match r with
      | Ok () -> Ok ()
      | Error _ as e ->
        Logs.warn (fun m -> m "Error: rolling back...");
        let* () = Db.rollback () in
        e
  in
  or_die 1 r

let input_ids =
  Caqti_type.unit ->* Builder_db.Rep.cstruct @@
  "SELECT DISTINCT input_id FROM build WHERE input_id IS NOT NULL"

let main_artifact_hash =
  Builder_db.Rep.cstruct ->*
  Caqti_type.tup3 Builder_db.Rep.cstruct Builder_db.Rep.uuid Caqti_type.string @@
  {|
    SELECT a.sha256, b.uuid, j.name FROM build_artifact a, build b, job j
    WHERE b.input_id = ? AND a.id = b.main_binary AND b.job = j.id
  |}

let verify_input_id () dbpath =
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* input_ids = Db.collect_list input_ids () in
    List.fold_left (fun acc input_id ->
        let* () = acc in
        let+ hashes = Db.collect_list main_artifact_hash input_id in
        match hashes with
        | (h, uuid, jobname) :: tl ->
          List.iter (fun (h', uuid', _) ->
              if Cstruct.equal h h' then
                ()
              else
                Logs.warn (fun m -> m "job %s input id %a with two different hashes (%a, %a), build %a and %a"
                              jobname Cstruct.hexdump_pp input_id
                              Cstruct.hexdump_pp h Cstruct.hexdump_pp h'
                              Uuidm.pp uuid Uuidm.pp uuid'))
            tl
        | [] -> ())
      (Ok ()) input_ids
  in
  or_die 1 r

let num_build_artifacts =
  Caqti_type.unit ->! Caqti_type.int @@
  "SELECT count(*) FROM build_artifact"

let build_artifacts : (unit, string * Uuidm.t * (Fpath.t * Fpath.t * Cstruct.t * int64), [ `One | `Zero | `Many ]) Caqti_request.t =
  Caqti_type.unit ->*
  Caqti_type.(tup3 string Builder_db.Rep.uuid
                (tup4 Builder_db.Rep.fpath Builder_db.Rep.fpath Builder_db.Rep.cstruct int64))
  @@
  {| SELECT job.name, b.uuid, a.filepath, a.localpath, a.sha256, a.size
     FROM build_artifact a, build b, job
     WHERE a.build = b.id AND b.job = job.id |}

let script_and_console : (unit, _, [`One | `Zero | `Many ]) Caqti_request.t =
  Caqti_type.unit ->*
  Caqti_type.(tup4 string Builder_db.Rep.uuid Builder_db.Rep.fpath Builder_db.Rep.fpath)
  @@
  {| SELECT job.name, b.uuid, b.console, b.script
     FROM build b, job
     WHERE job.id = b.job |}

module FpathSet = Set.Make(Fpath)

let files_in_dir dir =
  Bos.OS.Dir.fold_contents ~elements:`Files ~dotfiles:true
    (fun f acc ->
       let f = Option.get (Fpath.rem_prefix dir f) in
       FpathSet.add f acc)
    FpathSet.empty
    dir

let verify_data_dir () datadir =
  let files_in_filesystem = or_die 1 (files_in_dir (Fpath.v datadir)) in
  Logs.info (fun m -> m "files in filesystem: %d" (FpathSet.cardinal files_in_filesystem));
  let files_tracked = ref (FpathSet.singleton (Fpath.v "builder.sqlite3")) in
  let dbpath = datadir ^ "/builder.sqlite3" in
  Logs.info (fun m -> m "connecting to %s" dbpath);
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* num_build_artifacts = Db.find num_build_artifacts () in
    Logs.info (fun m -> m "total: %d artifacts" num_build_artifacts);
    let progress =
      let idx = ref 0 in
      fun () -> incr idx; if !idx mod 100 = 0 then Logs.info (fun m -> m "%d" !idx);
    in
    let verify_job_and_uuid ?fpath job uuid path =
      match Fpath.segs path with
      | job' :: uuid' :: tl ->
        if String.equal job job' then () else Logs.warn (fun m -> m "job names do not match: %s vs %s" job job');
        if String.equal (Uuidm.to_string uuid) uuid' then () else Logs.warn (fun m -> m "uuid does not match: %s vs %s" (Uuidm.to_string uuid) uuid');
        (match fpath, tl with
         | None, _ -> ()
         | Some f, "output" :: tl ->
           if Fpath.equal (Fpath.v (String.concat "/" tl)) f then
            ()
           else
            Logs.err (fun m -> m "path (%a) and fpath (%a) do not match" Fpath.pp path Fpath.pp f)
         | Some _, _ ->
           Logs.err (fun m -> m "path is not of form <job>/<uuid>/output/<filename>: %a" Fpath.pp path))
      | _ -> Logs.err (fun m -> m "path is not of form <job>/<uuid>/...: %a" Fpath.pp path)
    in
    let* () =
      Db.iter_s build_artifacts (fun (job, uuid, (fpath, lpath, sha, size)) ->
          progress ();
          verify_job_and_uuid ~fpath job uuid lpath;
          let abs_path = Fpath.(v datadir // lpath) in
          (match Bos.OS.File.read abs_path with
           | Error (`Msg msg) -> Logs.err (fun m -> m "file %a not present: %s" Fpath.pp abs_path msg)
           | Ok data ->
             files_tracked := FpathSet.add lpath !files_tracked;
             let s = Int64.of_int (String.length data) in
             if s <> size then Logs.err (fun m -> m "File %a has different size (in DB %Lu on disk %Lu)" Fpath.pp abs_path size s);
             let sh = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
             if not (Cstruct.equal sha sh) then Logs.err (fun m -> m "File %a has different hash (in DB %a on disk %a" Fpath.pp abs_path Cstruct.hexdump_pp sha Cstruct.hexdump_pp sh)) ;
          Ok ()
        ) ()
    in
    Db.iter_s script_and_console (fun (job, uuid, console, script) ->
       verify_job_and_uuid job uuid console;
       verify_job_and_uuid job uuid script;
       let console_file = Fpath.(v datadir // console)
       and script_file = Fpath.(v datadir // script)
       in
       let* _ = Bos.OS.File.must_exist console_file in
       let* _ = Bos.OS.File.must_exist script_file in
       files_tracked := FpathSet.add console (FpathSet.add script !files_tracked);
       Ok ()) ()
  in
  let files_untracked = FpathSet.diff files_in_filesystem !files_tracked in
  FpathSet.iter (fun f ->
      Logs.warn (fun m -> m "untracked file in filesystem: %a" Fpath.pp f))
    files_untracked;
  or_die 1 r

module Verify_cache_dir = struct 

  let verify_dir_exists d =
    let* dir_exists = Bos.OS.Dir.exists d in
    if dir_exists then Ok () else
      Error (`Msg (Fmt.str "The directory '%a' doesn't exist"
                     Fpath.pp d))

  let viz_types = [
    `Treemap;
    `Dependencies;
  ]

  let string_is_int s = match int_of_string_opt s with
    | None -> false
    | Some _ -> true
  
  let verify_cache_subdir ~cachedir d =
    match Bos.OS.Dir.exists Fpath.(cachedir // d) with
    | Ok false -> ()
    | Error _ ->
      Logs.warn (fun m ->
          m "Couldn't read file in cache: '%a'" Fpath.pp d)
    | Ok true ->
      let dir_str = Fpath.to_string d in
      let is_valid =
        viz_types |> List.exists (fun viz_type ->
            let viz_prefix = Builder_web.Viz_aux.viz_type_to_string viz_type in
            let prefix = viz_prefix ^ "_" in
            let has_prefix = String.starts_with ~prefix dir_str in
            let has_valid_ending =
              if not has_prefix then false else 
                let ending =
                  String.(sub dir_str
                            (length prefix)
                            (length dir_str - length prefix))
                in
                string_is_int ending
            in
            has_prefix && has_valid_ending
          )
      in
      if not is_valid then
        Logs.warn (fun m ->
            m "Invalid cache subdirectory name: '%s'" dir_str)

  let get_latest_viz_version viz_typ =
    let* v_str, run_status = begin match viz_typ with 
      | `Treemap ->
        let cmd = Bos.Cmd.(v "modulectomy" % "--version") in
        Bos.OS.Cmd.(cmd |> run_out |> out_string)
      | `Dependencies ->
        let cmd = Bos.Cmd.(v "opam-graph" % "--version") in
        Bos.OS.Cmd.(cmd |> run_out |> out_string)
    end in
    match run_status with
    | (cmd_info, `Exited 0) -> 
      begin try Ok (int_of_string v_str) with Failure _ ->
        let msg =
          Fmt.str "Couldn't parse latest version from %a: '%s'"
            Bos.Cmd.pp (Bos.OS.Cmd.run_info_cmd cmd_info)
            v_str
        in
        Error (`Msg msg)
      end
    | (cmd_info, _) ->
      let msg = 
        Fmt.str "Error running visualization cmd: '%a'"
          Bos.Cmd.pp (Bos.OS.Cmd.run_info_cmd cmd_info)
      in
      Error (`Msg msg)

  let verify_cachedir_contents cachedir =
    let* contents = Bos.OS.Dir.contents ~dotfiles:false ~rel:true cachedir in
    let contents =
      List.filter (fun f ->
          match Bos.OS.Dir.exists Fpath.(cachedir // f) with
          | Ok true -> true
          | Ok false ->
            Logs.warn (fun m -> m "Non-directory file '%a', ignoring" Fpath.pp f); false
          | Error `Msg err ->
            Logs.warn (fun m -> m "%s" err);
            false)
        contents
    in
    let () = contents |> List.iter (verify_cache_subdir ~cachedir) in
    let+ latest_versioned_subdirs =
      viz_types |> List.fold_left (fun acc viz_type ->
          let viz_prefix = Builder_web.Viz_aux.viz_type_to_string viz_type in
          let* acc = acc in
          let+ latest_viz_version = get_latest_viz_version viz_type in
          let path = Fpath.(
              cachedir / Fmt.str "%s_%d" viz_prefix latest_viz_version
            ) in
          (viz_prefix, path) :: acc
        ) (Ok [])
    in
    latest_versioned_subdirs |>
    List.iter (fun (viz_name, dir) ->
        match verify_dir_exists dir with
        | Error _ ->
          Logs.warn (fun m ->
              m "Latest versioned cache directory for %s doesn't exist: '%a'"
                viz_name Fpath.pp dir)
        | Ok () ->
          let done_file = Fpath.(dir / ".done") in
          match Bos.OS.File.exists done_file with
          | Ok true -> ()
          | Ok false ->
            Logs.warn (fun m ->
                m "'%a' doesn't exist (is batch-viz.sh running now?)"
                  Fpath.pp Fpath.(dir // done_file))
          | Error `Msg err ->
            Logs.warn (fun m -> m "%s" err))

  module Build = struct

    type t = {
      uuid : Uuidm.t;
      job_name : string;
      hash_opam_switch : Cstruct.t option;
      hash_debug_bin : Cstruct.t option;
    }

    let repr =
      let encode { uuid; job_name; hash_opam_switch; hash_debug_bin } =
        Ok (uuid, job_name, hash_opam_switch, hash_debug_bin) in
      let decode (uuid, job_name, hash_opam_switch, hash_debug_bin) =
        Ok { uuid; job_name; hash_opam_switch; hash_debug_bin }
      in
      Caqti_type.custom ~encode ~decode
        Caqti_type.(
          tup4
            Builder_db.Rep.uuid
            string
            (option Builder_db.Rep.cstruct)
            (option Builder_db.Rep.cstruct))

  end

  let builds_vizdeps_q =
    Caqti_type.unit ->* Build.repr @@ {|
      SELECT
        b.uuid,
        (SELECT name FROM job WHERE id = b.job) AS job_name,
        ba_opam_switch.sha256 hash_opam_switch,
        ba_debug_bin.sha256 hash_debug_bin
      FROM build AS b
      LEFT JOIN build_artifact AS ba_opam_switch ON
        ba_opam_switch.build = b.id
        AND ba_opam_switch.filepath = 'opam-switch'
      LEFT JOIN build_artifact AS ba_debug_bin ON
        ba_debug_bin.build = b.id
        AND ba_debug_bin.localpath LIKE '%.debug'
    |}

  let check_viz_nonempty ~cachedir ~viz_typ ~hash =
    let module Viz_aux = Builder_web.Viz_aux in
    let* latest_version =
      Viz_aux.get_viz_version_from_dirs ~cachedir ~viz_typ
    in
    let `Hex viz_input_hash = Hex.of_cstruct hash in
    let* viz_path =
      Viz_aux.choose_versioned_viz_path
        ~cachedir
        ~viz_typ
        ~viz_input_hash
        ~current_version:latest_version
    in
    let* path_info = Bos.OS.Path.stat viz_path in
    if path_info.Unix.st_size > 0 then Ok () else
      let msg = Fmt.str "Empty file: '%a'" Fpath.pp viz_path in
      Error (`Msg msg)

  let verify_viz_file_vizdeps ~cachedir build =
    match build.Build.hash_opam_switch with
    | None -> 
      Logs.warn (fun m ->
          m "%s: uuid '%a': Doesn't support dependencies viz because of \
             missing 'opam-switch'"
            build.job_name
            Uuidm.pp build.uuid)
    | Some hash_opam_switch ->
      match
        check_viz_nonempty
          ~cachedir 
          ~viz_typ:`Dependencies
          ~hash:hash_opam_switch
      with
      | Ok () -> ()
      | Error (`Msg err) ->
        Logs.warn (fun m ->
            m "%s: uuid '%a': %s"
              build.job_name
              Uuidm.pp build.uuid
              err)

  let verify_viz_file_viztreemap ~cachedir build =
    match build.Build.hash_debug_bin with
    | None -> ()
    | Some hash_debug_bin ->
      match
        check_viz_nonempty
          ~cachedir
          ~viz_typ:`Treemap
          ~hash:hash_debug_bin
      with 
      | Ok () -> ()
      | Error (`Msg err) ->
        Logs.warn (fun m ->
            m "%s: uuid '%a': %s"
              build.job_name
              Uuidm.pp build.uuid
              err)

  let verify_viz_files ~cachedir build =
    let () = verify_viz_file_vizdeps ~cachedir build in
    let () = verify_viz_file_viztreemap ~cachedir build in
    ()

  let has_completed ~cachedir ~viz_typ ~version =
    let module Viz_aux = Builder_web.Viz_aux in
    let viz_dir = Viz_aux.viz_dir
        ~cachedir
        ~viz_typ
        ~version
    in
    let* viz_dir_exists = Bos.OS.Dir.exists viz_dir in
    let* done_file_exists = Bos.OS.File.exists Fpath.(viz_dir / ".done") in
    Ok (viz_dir_exists && done_file_exists)

  let extract_hash ~viz_typ { Build.hash_debug_bin; hash_opam_switch; _ } =
    match viz_typ with
    | `Treemap -> hash_debug_bin
    | `Dependencies -> hash_opam_switch

  let verify_completeness ~cachedir ~viz_typ ~version build =
    let module Viz_aux = Builder_web.Viz_aux in
    match extract_hash ~viz_typ build with
    | None -> ()
    | Some input_hash ->
      let `Hex input_hash = Hex.of_cstruct input_hash in
      let viz_path = Viz_aux.viz_path
          ~cachedir
          ~viz_typ
          ~version
          ~input_hash
      in
      match Bos.OS.File.exists viz_path with
      | Ok true -> ()
      | Error (`Msg err) ->
        Logs.warn (fun m -> m "verify_completeness: Failure: %s" err)
      | Ok false ->
        Logs.warn (fun m ->
            m "%s: uuid '%a': Cache for visualization is marked as done, \
               but file '%a' is missing"
              build.Build.job_name
              Uuidm.pp build.Build.uuid
              Fpath.pp viz_path)

  type msg = [ `Msg of string ]
  
  let open_error_msg : ('a, msg) result -> ('a, [> msg]) result =
    function
    | Ok _ as v -> v
    | Error e -> Error (e : msg :> [> msg])
  
  let verify () datadir cachedir =
    let module Viz_aux = Builder_web.Viz_aux in
    begin
      let* datadir = Fpath.of_string datadir |> open_error_msg in
      let* cachedir = match cachedir with
        | Some d -> Fpath.of_string d |> open_error_msg 
        | None -> Ok Fpath.(datadir / "_cache")
      in
      let* () = verify_dir_exists cachedir in
      let* () = verify_cachedir_contents cachedir in
      let* (module Db : Caqti_blocking.CONNECTION) =
        let path = Fpath.(datadir / "builder.sqlite3" |> to_string) in
        let query = ["create", ["false"]] in
        connect (Uri.make ~scheme:"sqlite3" ~path ~query ())
      in
      let* viz_types_to_check =
        viz_types
        |> List.fold_left (fun acc viz_typ ->
            let* acc = acc in
            let* latest_version =
              Viz_aux.get_viz_version_from_dirs ~cachedir ~viz_typ
            in
            let* has_completed = has_completed ~cachedir
                ~viz_typ ~version:latest_version
            in
            if has_completed then
              Ok ((viz_typ, latest_version) :: acc)
            else
              Ok acc)
          (Ok [])
      in
      let+ () = Db.iter_s builds_vizdeps_q (fun build ->
          verify_viz_files ~cachedir build;
          List.iter (fun (viz_typ, version) ->
              verify_completeness ~cachedir ~viz_typ ~version build)
            viz_types_to_check;
          Ok ()
        ) ()
      in
      ()
    end
    |> or_die 1

end

module Asn = struct
  let decode_strict codec cs =
    match Asn.decode codec cs with
    | Ok (a, cs) ->
      if Cstruct.length cs = 0
      then Ok a
      else Error "trailing bytes"
    | Error (`Parse msg) -> Error ("parse error: " ^ msg)

  let projections_of asn =
    let c = Asn.codec Asn.der asn in
    (decode_strict c, Asn.encode c)

  let console =
    Asn.S.(sequence_of
             (sequence2
                (required ~label:"delta" int)
                (required ~label:"data" utf8_string)))

  let console_of_cs, console_to_cs = projections_of console
end

let console_of_string data =
  let lines = String.split_on_char '\n' data in
  (* remove last empty line *)
  let lines =
    match List.rev lines with
    | "" :: lines -> List.rev lines
    | _ -> lines
  in
  List.map (fun line ->
      match String.split_on_char ':' line with
      | ts :: tail ->
        let delta = float_of_string (String.sub ts 0 (String.length ts - 1)) in
        Int64.to_int (Duration.of_f delta), String.concat ":" tail
      | _ -> assert false)
    lines

let extract_full () datadir dest uuid =
  let dbpath = datadir ^ "/builder.sqlite3" in
  let r =
    let* (module Db : Caqti_blocking.CONNECTION) =
      connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    in
    let* uuid = Uuidm.of_string uuid |> Option.to_result ~none:(`Msg "bad uuid") in
    let* (build_id, build) =
      Db.find_opt Builder_db.Build.get_by_uuid uuid
      |> Fun.flip Result.bind (Option.to_result ~none:(`Msg "build not found"))
    in
    let { Builder_db.Build.start; finish; result;
          job_id; console; script; platform; _ } =
      build
    in
    let* job_name = Db.find Builder_db.Job.get job_id in
    let script_path = Fpath.(v datadir // script) in
    let* script = Bos.OS.File.read script_path in
    let job = { Builder.name = job_name; platform; script } in
    let console_path = Fpath.(v datadir // console) in
    let* console = Bos.OS.File.read console_path in
    let out = console_of_string console in
    let* artifacts = Db.collect_list Builder_db.Build_artifact.get_all_by_build build_id in
    let* data =
      List.fold_left (fun acc (_, { Builder_db.filepath; localpath; _ }) ->
          let* acc = acc in
          let* data = Bos.OS.File.read Fpath.(v datadir // localpath) in
          Ok ((filepath, data) :: acc))
        (Ok [])
        artifacts
    in
    let exec = (job, uuid, out, start, finish, result, data) in
    let cs = Builder.Asn.exec_to_cs exec in
    Bos.OS.File.write (Fpath.v dest) (Cstruct.to_string cs)
  in
  or_die 1 r

let help man_format cmds = function
  | None -> `Help (man_format, None)
  | Some cmd ->
    if List.mem cmd cmds
    then `Help (man_format, Some cmd)
    else `Error (true, "Unknown command: " ^ cmd)

let dbpath =
  let doc = "sqlite3 database path" in
  Cmdliner.Arg.(value &
                opt non_dir_file (Builder_system.default_datadir ^ "/builder.sqlite3") &
                info ~doc ["dbpath"])

let dbpath_new =
  let doc = "sqlite3 database path" in
  Cmdliner.Arg.(value &
                opt string (Builder_system.default_datadir ^ "/builder.sqlite3") &
                info ~doc ["dbpath"])

let datadir =
  let doc = "data directory" in
  Cmdliner.Arg.(value &
                opt dir Builder_system.default_datadir &
                info ~doc ["datadir"; "d"])

let cachedir =
  let doc = "cache directory" in
  Cmdliner.Arg.(value &
                opt (some dir) None &
                info ~doc ["cachedir"])

let jobname =
  let doc = "jobname" in
  Cmdliner.Arg.(required &
                pos 0 (some string) None &
                info ~doc ~docv:"JOBNAME" [])

let username =
  let doc = "username" in
  Cmdliner.Arg.(required &
                pos 0 (some string) None &
                info ~doc ~docv:"USERNAME" [])

let password_iter =
  let doc = "password hash count" in
  Cmdliner.Arg.(value &
                opt (some int) None &
                info ~doc ["hash-count"])

let scrypt_n =
  let doc = "scrypt n parameter" in
  Cmdliner.Arg.(value &
                opt (some int) None &
                info ~doc ["scrypt-n"])

let scrypt_r =
  let doc = "scrypt r parameter" in
  Cmdliner.Arg.(value &
                opt (some int) None &
                info ~doc ["scrypt-r"])

let scrypt_p =
  let doc = "scrypt p parameter" in
  Cmdliner.Arg.(value &
                opt (some int) None &
                info ~doc ["scrypt-p"])

let unrestricted =
  let doc = "unrestricted user" in
  Cmdliner.Arg.(value & flag & info ~doc [ "unrestricted" ])

let job =
  let doc = "job" in
  Cmdliner.Arg.(required &
                pos 1 (some string) None &
                info ~doc ~docv:"JOB" [])

let build =
  let doc = "build uuid" in
  Cmdliner.Arg.(required &
                pos 0 (some string) None &
                info ~doc ~docv:"BUILD" [])

let full_dest =
  let doc = "path to write build file" in
  Cmdliner.Arg.(value & opt string "full" &
                info ~doc ["dest"])

let setup_log =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ());
    Logs.debug (fun m -> m "Set log level %s" (Logs.level_to_string level))
  in
  Cmdliner.Term.(const setup_log $ Logs_cli.level ())

open Cmdliner

let migrate_cmd =
  let doc = "create database and add tables" in
  let term = Term.(const migrate $ setup_log $ dbpath_new) in
  let info = Cmd.info ~doc "migrate" in
  Cmd.v info term

let user_add_cmd =
  let doc = "add a user" in
  let term = Term.(
      const user_add $ setup_log $ dbpath $ scrypt_n $ scrypt_r $ scrypt_p
      $ username $ unrestricted) in
  let info = Cmd.info ~doc "user-add" in
  Cmd.v info term

let user_update_cmd =
  let doc = "update a user password" in
  let term = Term.(
      const user_update $ setup_log $ dbpath $ scrypt_n $ scrypt_r $ scrypt_p
      $ username $ unrestricted) in
  let info = Cmd.info ~doc "user-update" in
  Cmd.v info term

let user_remove_cmd =
  let doc = "remove a user" in
  let term = Term.(const user_remove $ setup_log $ dbpath $ username) in
  let info = Cmd.info ~doc "user-remove" in
  Cmd.v info term

let user_disable_cmd =
  let doc = "disable a user" in
  let term = Term.(const user_disable $ setup_log $ dbpath $ username) in
  let info = Cmd.info ~doc "user-disable" in
  Cmd.v info term

let user_list_cmd =
  let doc = "list all users" in
  let term = Term.(const user_list $ setup_log $ dbpath) in
  let info = Cmd.info ~doc "user-list" in
  Cmd.v info term

let access_add_cmd =
  let doc = "grant access to user and job" in
  let term = Term.(const access_add $ setup_log $ dbpath $ username $ job) in
  let info = Cmd.info ~doc "access-add" in
  Cmd.v info term

let access_remove_cmd =
  let doc = "remove access to user and job" in
  let term = Term.(const access_remove $ setup_log $ dbpath $ username $ job) in
  let info = Cmd.info ~doc "access-remove" in
  Cmd.v info term

let job_remove_cmd =
  let doc = "remove job and its associated builds and artifacts" in
  let term = Term.(const job_remove $ setup_log $ datadir $ jobname) in
  let info = Cmd.info ~doc "job-remove" in
  Cmd.v info term

let extract_full_cmd =
  let doc = "extract a build from the database" in
  let term = Term.(
      const extract_full $ setup_log $ datadir $ full_dest $ build) in
  let info = Cmd.info ~doc "extract-build" in
  Cmd.v info term

let verify_input_id_cmd =
  let doc = "verify that the main binary hash of all builds with the same \
             input are equal" in
  let term = Term.(const verify_input_id $ setup_log $ dbpath) in
  let info = Cmd.info ~doc "verify-input-id" in
  Cmd.v info term

let verify_data_dir_cmd =
  let doc = "verify that the data directory is consistent with the \
             build_artifact table" in
  let term = Term.(const verify_data_dir $ setup_log $ datadir) in
  let info = Cmd.info ~doc "verify-data-dir" in
  Cmd.v info term

let verify_cache_dir_cmd =
  let doc = "verify the cache directory" in
  let term = Term.(const Verify_cache_dir.verify $ setup_log $ datadir $ cachedir) in
  let info = Cmd.info ~doc "verify-cache-dir" in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "Command to get help on" in
    Arg.(value & pos 0 (some string) None & info ~doc ~docv:"COMMAND" [])
  in
  let doc = "Builder database help" in
  let term = Term.(ret (const help $ Arg.man_format $ choice_names $ topic)) in
  let info = Cmd.info ~doc "help" in
  Cmd.v info term

let default_cmd, default_info =
  let doc = "Builder database command" in
  Term.(ret (const help $ Arg.man_format $ choice_names $ const None)),
  Cmd.info ~doc "builder-db"

let () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Cmdliner.Cmd.group
    ~default:default_cmd default_info
    [ help_cmd; migrate_cmd;
      user_add_cmd; user_update_cmd; user_remove_cmd; user_list_cmd;
      user_disable_cmd;
      access_add_cmd; access_remove_cmd; job_remove_cmd;
      verify_input_id_cmd;
      verify_data_dir_cmd;
      verify_cache_dir_cmd;
      extract_full_cmd ]
  |> Cmdliner.Cmd.eval
  |> exit

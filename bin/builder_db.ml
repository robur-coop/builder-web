open Rresult.R.Infix

let or_die exit_code = function
  | Ok r -> r
  | Error (`Msg msg) ->
    Format.eprintf "Error: %s" msg;
    exit exit_code
  | Error (#Caqti_error.t as e) ->
    Format.eprintf "Database error: %a" Caqti_error.pp e;
    exit exit_code

let foreign_keys =
  Caqti_request.exec
    Caqti_type.unit
    "PRAGMA foreign_keys = ON"

let defer_foreign_keys =
  Caqti_request.exec
    Caqti_type.unit
    "PRAGMA defer_foreign_keys = ON"

let connect uri =
  Caqti_blocking.connect uri >>= fun (module Db : Caqti_blocking.CONNECTION) ->
  Db.exec foreign_keys () >>= fun () ->
  Db.exec defer_foreign_keys () >>= fun () ->
  Ok (module Db : Caqti_blocking.CONNECTION)

let do_migrate dbpath =
  connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ())
  >>= fun (module Db : Caqti_blocking.CONNECTION) ->
  List.fold_left
    (fun r migrate ->
       r >>= fun () ->
       Logs.debug (fun m -> m "Executing migration query: %a" Caqti_request.pp migrate);
       Db.exec migrate ())
    (Ok ())
    Builder_db.migrate

let migrate () dbpath =
  or_die 1 (do_migrate dbpath)

let user_mod action dbpath scrypt_n scrypt_r scrypt_p username unrestricted =
  let scrypt_params = Builder_web_auth.scrypt_params ?scrypt_n ?scrypt_r ?scrypt_p () in
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
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
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION) ->
    Db.iter_s Builder_db.User.get_all
      (fun username -> Ok (print_endline username))
      ()
  in
  or_die 1 r

let user_remove () dbpath username =
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.exec Builder_db.Access_list.remove_all_by_username username >>= fun () ->
    Db.exec Builder_db.User.remove_user username
  in
  or_die 1 r

let user_disable () dbpath username =
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.exec Builder_db.Access_list.remove_all_by_username username >>= fun () ->
    Db.find_opt Builder_db.User.get_user username >>= function
    | None -> Error (`Msg "user not found")
    | Some (_, user_info) ->
      let password_hash = `Scrypt (Cstruct.empty, Cstruct.empty, Builder_web_auth.scrypt_params ()) in
      let user_info = { user_info with password_hash ; restricted = true } in
      Db.exec Builder_db.User.update_user user_info
  in
  or_die 1 r

let access_add () dbpath username jobname =
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.find_opt Builder_db.User.get_user username >>=
    Option.to_result ~none:(`Msg "unknown user") >>= fun (user_id, _) ->
    Db.find_opt Builder_db.Job.get_id_by_name jobname >>=
    Option.to_result ~none:(`Msg "job not found") >>= fun job_id ->
    Db.exec Builder_db.Access_list.add (user_id, job_id)
   in
   or_die 1 r

let access_remove () dbpath username jobname =
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.find_opt Builder_db.User.get_user username >>=
    Option.to_result ~none:(`Msg "unknown user") >>= fun (user_id, _) ->
    Db.find_opt Builder_db.Job.get_id_by_name jobname >>=
    Option.to_result ~none:(`Msg "job not found") >>= fun job_id ->
    Db.exec Builder_db.Access_list.remove (user_id, job_id)
   in
   or_die 1 r

let job_remove () datadir jobname =
  let dbpath = datadir ^ "/builder.sqlite3" in
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.find_opt Builder_db.Job.get_id_by_name jobname >>= function
    | None ->
      Logs.info (fun m -> m "Job %S doesn't exist or has already been removed." jobname);
      Ok ()
    | Some job_id ->
      Db.start () >>= fun () ->
      Db.exec defer_foreign_keys () >>= fun () ->
      let r =
        Db.collect_list Builder_db.Build.get_all job_id >>= fun builds ->
        List.fold_left (fun r (build_id, build) ->
            r >>= fun () ->
            let dir = Fpath.(v datadir / jobname / Uuidm.to_string build.Builder_db.Build.uuid) in
            (match Bos.OS.Dir.delete ~recurse:true dir with
            | Ok _ -> ()
            | Error `Msg e -> Logs.warn (fun m -> m "failed to remove build directory %a: %s" Fpath.pp dir e));
            Db.exec Builder_db.Build_artifact.remove_by_build build_id >>= fun () ->
            Db.exec Builder_db.Build.remove build_id)
          (Ok ())
          builds >>= fun () ->
        Db.exec Builder_db.Job.remove job_id >>= fun () ->
        Db.commit ()
      in
      match r with
      | Ok () -> Ok ()
      | Error _ as e ->
        Logs.warn (fun m -> m "Error: rolling back...");
        Db.rollback () >>= fun () ->
        e
  in
  or_die 1 r

let input_ids =
   Caqti_request.collect
     Caqti_type.unit
     Builder_db.Rep.cstruct
     "SELECT DISTINCT input_id FROM build WHERE input_id IS NOT NULL"

let main_artifact_hash =
   Caqti_request.collect
     Builder_db.Rep.cstruct
     (Caqti_type.tup3 Builder_db.Rep.cstruct Builder_db.Rep.uuid Caqti_type.string)
     {|
       SELECT a.sha256, b.uuid, j.name FROM build_artifact a, build b, job j
       WHERE b.input_id = ? AND a.id = b.main_binary AND b.job = j.id
     |}

let verify_input_id () dbpath =
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.collect_list input_ids () >>= fun input_ids ->
    List.fold_left (fun acc input_id ->
      acc >>= fun () ->
      Db.collect_list main_artifact_hash input_id >>| fun hashes ->
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
  Caqti_request.find
    Caqti_type.unit
    Caqti_type.int
    "SELECT count(*) FROM build_artifact"

let build_artifacts : (unit, string * Uuidm.t * (Fpath.t * Fpath.t * Cstruct.t * int64), [ `One | `Zero | `Many ]) Caqti_request.t =
  Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup3 string Builder_db.Rep.uuid (tup4 Builder_db.Rep.fpath Builder_db.Rep.fpath Builder_db.Rep.cstruct int64))
    {| SELECT job.name, b.uuid, a.filepath, a.localpath, a.sha256, a.size
       FROM build_artifact a, build b, job
       WHERE a.build = b.id AND b.job = job.id |}

let verify_data_dir () datadir =
  let dbpath = datadir ^ "/builder.sqlite3" in
  Logs.info (fun m -> m "connecting to %s" dbpath);
  let r =
    connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.find num_build_artifacts () >>= fun num_build_artifacts ->
    Logs.info (fun m -> m "total: %d artifacts" num_build_artifacts);
    let progress =
      let idx = ref 0 in
      fun () -> incr idx; if !idx mod 100 = 0 then Logs.info (fun m -> m "%d" !idx);
    in
    Db.iter_s build_artifacts (fun (job, uuid, (fpath, lpath, sha, size)) ->
        progress ();
        (match Fpath.segs lpath with
         | job' :: uuid' :: "output" :: tl ->
           if String.equal job job' then () else Logs.warn (fun m -> m "job names do not match: %s vs %s" job job');
           if String.equal (Uuidm.to_string uuid) uuid' then () else Logs.warn (fun m -> m "uuid does not match: %s vs %s" (Uuidm.to_string uuid) uuid');
           if Fpath.equal (Fpath.v (String.concat "/" tl)) fpath then
             ()
           else
             Logs.err (fun m -> m "lpath (%a) and fpath (%a) do not match" Fpath.pp lpath Fpath.pp lpath)
         | _ -> Logs.err (fun m -> m "lpath is not of form <job>/<uuid>/output/<filename>: %a" Fpath.pp lpath));
        let abs_path = Fpath.(v datadir // lpath) in
        (match Bos.OS.File.read abs_path with
         | Error (`Msg msg) -> Logs.err (fun m -> m "file %a not present: %s" Fpath.pp abs_path msg)
         | Ok data ->
           let s = Int64.of_int (String.length data) in
           if s <> size then Logs.err (fun m -> m "File %a has different size (in DB %Lu on disk %Lu)" Fpath.pp abs_path size s);
           let sh = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
           if not (Cstruct.equal sha sh) then Logs.err (fun m -> m "File %a has different hash (in DB %a on disk %a" Fpath.pp abs_path Cstruct.hexdump_pp sha Cstruct.hexdump_pp sh)) ;
        Ok ()
      ) ()
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
                opt non_dir_file "/var/db/builder-web/builder.sqlite3" &
                info ~doc ["dbpath"])

let dbpath_new =
  let doc = "sqlite3 database path" in
  Cmdliner.Arg.(value &
                opt string "/var/db/builder-web/builder.sqlite3" &
                info ~doc ["dbpath"])

let datadir =
  let doc = "data directory" in
  Cmdliner.Arg.(value &
                opt dir "/var/db/builder-web/" &
                info ~doc ["datadir"])

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

let setup_log =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ());
    Logs.debug (fun m -> m "Set log level %s" (Logs.level_to_string level))
  in
  Cmdliner.Term.(const setup_log $ Logs_cli.level ())

let migrate_cmd =
  let doc = "create database and add tables" in
  Cmdliner.Term.(pure migrate $ setup_log $ dbpath_new),
  Cmdliner.Term.info ~doc "migrate"

let user_add_cmd =
  let doc = "add a user" in
  (Cmdliner.Term.(pure user_add $ setup_log $ dbpath $ scrypt_n $ scrypt_r $ scrypt_p $ username $ unrestricted),
   Cmdliner.Term.info ~doc "user-add")

let user_update_cmd =
  let doc = "update a user password" in
  (Cmdliner.Term.(pure user_update $ setup_log $ dbpath $ scrypt_n $ scrypt_r $ scrypt_p $ username $ unrestricted),
   Cmdliner.Term.info ~doc "user-update")

let user_remove_cmd =
  let doc = "remove a user" in
  (Cmdliner.Term.(pure user_remove $ setup_log $ dbpath $ username),
   Cmdliner.Term.info ~doc "user-remove")

let user_disable_cmd =
  let doc = "disable a user" in
  (Cmdliner.Term.(pure user_disable $ setup_log $ dbpath $ username),
   Cmdliner.Term.info ~doc "user-disable")

let user_list_cmd =
  let doc = "list all users" in
  (Cmdliner.Term.(pure user_list $ setup_log $ dbpath),
   Cmdliner.Term.info ~doc "user-list")

let access_add_cmd =
  let doc = "grant access to user and job" in
  (Cmdliner.Term.(pure access_add $ setup_log $ dbpath $ username $ job),
   Cmdliner.Term.info ~doc "access-add")

let access_remove_cmd =
  let doc = "remove access to user and job" in
  (Cmdliner.Term.(pure access_remove $ setup_log $ dbpath $ username $ job),
   Cmdliner.Term.info ~doc "access-remove")

let job_remove_cmd =
  let doc = "remove job and its associated builds and artifacts" in
  (Cmdliner.Term.(pure job_remove $ setup_log $ datadir $ jobname),
   Cmdliner.Term.info ~doc "job-remove")

let verify_input_id_cmd =
  let doc = "verify that the main binary hash of all builds with the same input are equal" in
  (Cmdliner.Term.(pure verify_input_id $ setup_log $ dbpath),
   Cmdliner.Term.info ~doc "verify-input-id")

let verify_data_dir_cmd =
  let doc = "verify that the data directory is consistent with the build_artifact table" in
  (Cmdliner.Term.(pure verify_data_dir $ setup_log $ datadir),
   Cmdliner.Term.info ~doc "verify-data-dir")

let help_cmd =
  let topic =
    let doc = "Command to get help on" in
    Cmdliner.Arg.(value & pos 0 (some string) None & info ~doc ~docv:"COMMAND" [])
  in
  let doc = "Builder database help" in
  Cmdliner.Term.(ret (const help $ man_format $ choice_names $ topic)),
  Cmdliner.Term.info ~doc "help"

let default_cmd =
  let doc = "Builder database command" in
  Cmdliner.Term.(ret (const help $ man_format $ choice_names $ const None)),
  Cmdliner.Term.info ~doc "builder-db"

let () =
  Mirage_crypto_rng_unix.initialize ();
  Cmdliner.Term.eval_choice
    default_cmd
    [help_cmd; migrate_cmd;
     user_add_cmd; user_update_cmd; user_remove_cmd; user_list_cmd; user_disable_cmd;
     access_add_cmd; access_remove_cmd; job_remove_cmd;
     verify_input_id_cmd; verify_data_dir_cmd ]
  |> Cmdliner.Term.exit

open Rresult.R.Infix

let pp_error ppf = function
  | #Caqti_error.load_or_connect | #Caqti_error.call_or_retrieve as e ->
    Caqti_error.pp ppf e
  | `Wrong_version (application_id, user_version) ->
    Format.fprintf ppf "wrong version { application_id: %ld, user_version: %Ld }"
      application_id user_version
  | `Msg m ->
    Format.fprintf ppf "%s" m

let or_die exit_code = function
  | Ok r -> r
  | Error e ->
    Format.eprintf "Database error: %a" pp_error e;
    exit exit_code

let do_database_action action () datadir =
  let datadir = Fpath.v datadir in
  let dbpath = Fpath.(datadir / "builder.sqlite3") in
  Logs.debug (fun m -> m "Connecting to database...");
  let ((module Db : Caqti_blocking.CONNECTION) as conn) =
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:(Fpath.to_string dbpath) ~query:["create", ["false"]] ())
    |> or_die 1
  in
  Logs.debug (fun m -> m "Connected!");
  let r =
    Db.start () >>= fun () ->
    Logs.debug (fun m -> m "Started database transaction");
    match action datadir conn with
    | Ok () ->
      Logs.debug (fun m -> m "Committing database transaction");
      Db.commit ()
    | Error _ as e ->
      Logs.debug (fun m -> m "Rolling back database transaction");
      Db.rollback () >>= fun () ->
      e
  in
  or_die 2 r

let help man_format migrations = function
  | None -> `Help (man_format, None)
  | Some migration ->
    if List.mem migration migrations
    then `Help (man_format, Some migration)
    else `Error (true, "Unknown migration: " ^ migration)

let datadir =
  let doc = "data directory containing builder.sqlite3 and data files" in
  Cmdliner.Arg.(value &
                opt dir "/var/db/builder-web/" &
                info ~doc ["datadir"])

let setup_log =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ());
  in
  Cmdliner.Term.(const setup_log $ Logs_cli.level ())

let m20210126 =
  let doc = "Adds a column 'main_binary' in 'build' (2021-01-26)" in
  Cmdliner.Term.(const do_database_action $ const M20210126.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-01-26"

let r20210126 =
  let doc = "Rollback 'main_binary' in 'build' (2021-01-26)" in
  Cmdliner.Term.(const do_database_action $ const M20210126.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-01-26"

let m20210202 =
  let doc = "Adds an index 'job_build_idx' on 'build' (2021-02-02)" in
  Cmdliner.Term.(const do_database_action $ const M20210202.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-02-02"

let r20210202 =
  let doc = "Rollback index 'job_build_idx' on 'build' (2021-02-02)" in
  Cmdliner.Term.(const do_database_action $ const M20210202.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-02-02"

let m20210216 =
  let doc = "Changes 'user' for scrypt hashed passwords (NB: Destructive!!) (2021-02-16)" in
  Cmdliner.Term.(const do_database_action $ const M20210216.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-02-16"

let r20210216 =
  let doc = "Rollback scrypt hashed passwords (NB: Destructive!!) (2021-02-16)" in
  Cmdliner.Term.(const do_database_action $ const M20210216.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-02-16"

let m20210218 =
  let doc = "Adds column 'size' to 'build_file' and 'build_artifact' (2021-02-18)" in
  Cmdliner.Term.(const do_database_action $ const M20210218.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-02-18"

let r20210218 =
  let doc = "Roll back column 'size' in 'build_file' and 'build_artifact' (2021-02-18)" in
  Cmdliner.Term.(const do_database_action $ const M20210218.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-02-18"

let f20210308 =
  let doc = "Remove broken builds as fixed in commit a57798f4c02eb4d528b90932ec26fb0b718f1a13. \
    Note that the files on disk have to be removed manually." in
  Cmdliner.Term.(const do_database_action $ const M20210308.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-03-08"

let m20210427 =
  let doc = "Adds an index 'idx_build_job_start' on 'build' (2021-04-27)" in
  Cmdliner.Term.(const do_database_action $ const M20210427.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-04-27"

let r20210427 =
  let doc = "Rollback index 'idx_build_job_start'' on 'build' (2021-04-27)" in
  Cmdliner.Term.(const do_database_action $ const M20210427.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-04-27"

let m20210531 =
  let doc = "Remove datadir from build_artifact.localpath" in
  Cmdliner.Term.(const do_database_action $ const M20210531.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-05-31"

let r20210531 =
  let doc = "Add datadir to build_artifact.localpath" in
  Cmdliner.Term.(const do_database_action $ const M20210531.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-05-31"

let m20210602 =
  let doc = "build.main_binary foreign key" in
  Cmdliner.Term.(const do_database_action $ const M20210602.migrate $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "migrate-2021-06-02"

let r20210602 =
  let doc = "build.main_binary filepath" in
  Cmdliner.Term.(const do_database_action $ const M20210602.rollback $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "rollback-2021-06-02"

let help_cmd =
  let topic =
    let doc = "Migration to get help on" in
    Cmdliner.Arg.(value & pos 0 (some string) None & info ~doc ~docv:"MIGRATION" [])
  in
  let doc = "Builder migration help" in
  Cmdliner.Term.(ret (const help $ man_format $ choice_names $ topic)),
  Cmdliner.Term.info ~doc "help"

let default_cmd =
  let doc = "Builder migration command" in
  Cmdliner.Term.(ret (const help $ man_format $ choice_names $ const None)),
  Cmdliner.Term.info ~doc "builder-migrations"

let () =
  Cmdliner.Term.eval_choice
    default_cmd
    [ help_cmd;
      m20210126; r20210126;
      m20210202; r20210202;
      m20210216; r20210216;
      m20210218; r20210218;
      f20210308;
      m20210427; r20210427;
      m20210531; r20210531;
      m20210602; r20210602;
    ]
  |> Cmdliner.Term.exit

open Rresult.R.Infix

let pp_error ppf = function
  | #Caqti_error.load_or_connect | #Caqti_error.call_or_retrieve as e ->
    Caqti_error.pp ppf e
  | `Wrong_version (application_id, user_version) ->
    Format.fprintf ppf "wrong version { application_id: %ld, user_version: %Ld }"
      application_id user_version

let or_die exit_code = function
  | Ok r -> r
  | Error e ->
    Format.eprintf "Database error: %a" pp_error e;
    exit exit_code

let do_database_action action () dbpath =
  Logs.debug (fun m -> m "Connecting to database...");
  let ((module Db : Caqti_blocking.CONNECTION) as conn) =
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    |> or_die 1
  in
  Logs.debug (fun m -> m "Connected!");
  let r =
    Db.start () >>= fun () ->
    Logs.debug (fun m -> m "Started database transaction");
    match action conn with
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

let dbpath =
  let doc = "sqlite3 database path" in
  Cmdliner.Arg.(value &
                opt non_dir_file "/var/db/builder-web/builder.sqlite3" &
                info ~doc ["dbpath"])

let setup_log =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ());
  in
  Cmdliner.Term.(const setup_log $ Logs_cli.level ())

let m20210126 =
  let doc = "Adds a column 'main_binary' in 'build' (2021-01-26)" in
  Cmdliner.Term.(const do_database_action $ const M20210126.migrate $ setup_log $ dbpath),
  Cmdliner.Term.info ~doc "migrate-2021-01-26"

let r20210126 =
  let doc = "Rollback 'main_binary' in 'build' (2021-01-26)" in
  Cmdliner.Term.(const do_database_action $ const M20210126.rollback $ setup_log $ dbpath),
  Cmdliner.Term.info ~doc "rollback-2021-01-26"

let m20210202 =
  let doc = "Adds an index 'job_build_idx' on 'build' (2021-02-02)" in
  Cmdliner.Term.(const do_database_action $ const M20210202.migrate $ setup_log $ dbpath),
  Cmdliner.Term.info ~doc "migrate-2021-02-02"

let r20210202 =
  let doc = "Rollback index 'job_build_idx' on 'build' (2021-02-02)" in
  Cmdliner.Term.(const do_database_action $ const M20210202.rollback $ setup_log $ dbpath),
  Cmdliner.Term.info ~doc "rollback-2021-02-02"

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
    ]
  |> Cmdliner.Term.exit

type action = Fpath.t -> Caqti_blocking.connection ->
  (unit, [ Caqti_error.call_or_retrieve | `Wrong_version of int32 * int64 | `Msg of string ]) result

module type MIGRATION = sig
  val new_version : int64
  val old_version : int64
  val identifier : string
  val migrate_doc : string
  val rollback_doc : string
  val migrate : action
  val rollback : action
end


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
  let ( let* ) = Result.bind in
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
    let* () = Db.start () in
    Logs.debug (fun m -> m "Started database transaction");
    match action datadir conn with
    | Ok () ->
      Logs.debug (fun m -> m "Committing database transaction");
      Db.commit ()
    | Error _ as e ->
      Logs.debug (fun m -> m "Rolling back database transaction");
      let* () = Db.rollback () in
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

let actions (module M : MIGRATION) =
  let c s = s ^ "-" ^ M.identifier in
  let v doc from_ver to_ver = Printf.sprintf "%s (DB version %Ld -> %Ld)" doc from_ver to_ver in
  [
    (Cmdliner.Term.(const do_database_action $ const M.migrate $ setup_log $ datadir),
     Cmdliner.Term.info ~doc:(v M.migrate_doc M.old_version M.new_version)
       (c "migrate"));
    (Cmdliner.Term.(const do_database_action $ const M.rollback $ setup_log $ datadir),
     Cmdliner.Term.info ~doc:(v M.rollback_doc M.new_version M.old_version)
       (c "rollback"));
  ]

let f20210308 =
  let doc = "Remove broken builds as fixed in commit a57798f4c02eb4d528b90932ec26fb0b718f1a13. \
    Note that the files on disk have to be removed manually." in
  Cmdliner.Term.(const do_database_action $ const M20210308.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-03-08"

let f20210707a =
  let doc = "Remove orb.deb and orb.txz that ended up in the build." in
  Cmdliner.Term.(const do_database_action $ const M20210707a.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-07-07a"

let f20210707b =
  let doc = "Move *.deb.debug to bin/*.deb and remove the earlier bin/*.deb. Adjust main_binary of build." in
  Cmdliner.Term.(const do_database_action $ const M20210707b.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-07-07b"

let f20210707c =
  let doc = "Strip bin/*.{hvt,xen} if no *.{hvt,xen} exists. Adjust build_artifact table and main_binary of build." in
  Cmdliner.Term.(const do_database_action $ const M20210707c.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-07-07c"

let f20210707d =
  let doc = "Remove ./ from filepath." in
  Cmdliner.Term.(const do_database_action $ const M20210707d.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-07-07d"

let f20210712b =
  let doc = "Remove build-hashes and README from artifacts." in
  Cmdliner.Term.(const do_database_action $ const M20210712b.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-07-12b"

let f20210910 =
  let doc = "Undo builds with script and console mixed up." in
  Cmdliner.Term.(const do_database_action $ const M20210910.fixup $ setup_log $ datadir),
  Cmdliner.Term.info ~doc "fixup-2021-09-10"

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
    (List.concat [
        [ help_cmd ];
        actions (module M20210126);
        actions (module M20210202);
        actions (module M20210216);
        actions (module M20210218);
        [ f20210308 ];
        actions (module M20210427);
        actions (module M20210531);
        actions (module M20210602);
        actions (module M20210608);
        actions (module M20210609);
        actions (module M20210625);
        actions (module M20210629);
        actions (module M20210630);
        actions (module M20210701);
        actions (module M20210706);
        [ f20210707a ];
        [ f20210707b ];
        [ f20210707c ];
        [ f20210707d ];
        actions (module M20210712a);
        [ f20210712b ];
        actions (module M20210712c);
        [ f20210910 ];
      ])
  |> Cmdliner.Term.exit

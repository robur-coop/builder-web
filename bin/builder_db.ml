open Rresult.R.Infix

let or_die exit_code = function
  | Ok r -> r
  | Error (`Msg msg) ->
    Format.eprintf "Error: %s" msg;
    exit exit_code
  | Error (#Caqti_error.t as e) ->
    Format.eprintf "Database error: %a" Caqti_error.pp e;
    exit exit_code

let do_migrate dbpath =
  Caqti_blocking.connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ())
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
    Caqti_blocking.connect
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
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION) ->
    Db.iter_s Builder_db.User.get_all
      (fun username -> Ok (print_endline username))
      ()
  in
  or_die 1 r

let user_remove () dbpath username =
  let r =
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.exec Builder_db.Access_list.remove_all_by_username username >>= fun () ->
    Db.exec Builder_db.User.remove_user username
  in
  or_die 1 r

let access_add () dbpath username jobname =
  let r =
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.find_opt Builder_db.User.get_user username >>= function
    | None -> Error (`Msg "unknown user")
    | Some (user_id, _) ->
      Db.find Builder_db.Job.get_id_by_name jobname >>= fun job_id ->
      Db.exec Builder_db.Access_list.add (user_id, job_id)
   in
   or_die 1 r

 let access_remove () dbpath username jobname =
  let r =
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    Db.find_opt Builder_db.User.get_user username >>= function
    | None -> Error (`Msg "unknown user")
    | Some (user_id, _) ->
      Db.find Builder_db.Job.get_id_by_name jobname >>= fun job_id ->
      Db.exec Builder_db.Access_list.remove (user_id, job_id)
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
     user_add_cmd; user_update_cmd; user_remove_cmd; user_list_cmd;
     access_add_cmd; access_remove_cmd]
  |> Cmdliner.Term.exit

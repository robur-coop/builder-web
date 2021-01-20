open Rresult.R.Infix

let or_die exit_code = function
  | Ok r -> r
  | Error e ->
    Format.eprintf "Database error: %a" Caqti_error.pp e;
    exit exit_code

let save_data outputdir (filepath, data) =
  let localpath = Fpath.(outputdir // filepath) in
  (* FIXME: return an error?! *)
  let () =
    match Bos.OS.File.exists localpath with
    | Ok false ->
      Logs.warn (fun m -> m "artifact file %a does not exist in %a"
                    Fpath.pp filepath Fpath.pp outputdir)
    | Error (`Msg e) ->
      Logs.warn (fun m -> m "artifact file error %a: %s"
                    Fpath.pp localpath e)
    | Ok true -> ()
  in
  (filepath, localpath, data)

let get_by_uuid uuid (module Db : Caqti_blocking.CONNECTION) =
  Uuidm.of_string uuid |> Option.to_result ~none:"bad uuid" >>= fun uuid ->
  Db.find_opt Builder_db.Build.get_by_uuid uuid
  |> Result.map_error (fun e ->
      Fmt.strf "Error getting build %a: %a" Uuidm.pp uuid Caqti_error.pp e)

let db_add_build (job, uuid, console, start, finish, result, artifacts)
    (input_files : (Fpath.t * Fpath.t * string) list)
    (module Db : Caqti_blocking.CONNECTION) =
  let open Builder_db in
  let job_name = job.Builder.name in
  Db.exec Job.try_add job_name >>= fun () ->
  Db.find Job.get_id_by_name job_name >>= fun job_id ->
  Db.exec Build.add { Build.uuid; start; finish; result; console;
                  script = job.Builder.script; job_id } >>= fun () ->
  Db.find last_insert_rowid () >>= fun id ->
  List.fold_left
    (fun r (filepath, localpath, data) ->
       r >>= fun () ->
       let sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
       Db.exec Build_artifact.add ({ filepath; localpath; sha256 }, id))
    (Ok ())
    artifacts >>= fun () ->
  List.fold_left
    (fun r (filepath, localpath, data) ->
       r >>= fun () ->
       let sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
       Db.exec Build_file.add ({ filepath; localpath; sha256 }, id))
    (Ok ())
    input_files

let add_build conn builddir =
  let f = Fpath.(builddir / "full") in
  let outputdir = Fpath.(builddir / "output") in
  let inputdir = Fpath.(builddir / "input") in
  let uuid = Fpath.basename builddir in
  match get_by_uuid uuid conn with
  | Error e -> Logs.warn (fun m -> m "%s" e)
  | Ok (Some _) -> Logs.debug (fun m -> m "Skipping %a, already in database" Fpath.pp builddir)
  | Ok None ->
    Logs.debug (fun m -> m "Adding build %a" Fpath.pp builddir);
    match Bos.OS.File.read f with
    | Error (`Msg e) ->
      Logs.warn (fun m -> m "Error getting build %a: %s"
                    Fpath.pp builddir e)
    | Ok contents ->
      match Builder.Asn.exec_of_cs (Cstruct.of_string contents) with
      | Error (`Msg e) ->
        Logs.warn (fun m -> m "Error parsing build file %a: %s"
                      Fpath.pp f e)
      | Ok (job, uuid, console, start, finish, result, data) ->
        let data = List.map (save_data outputdir) data in
        let input_files = List.map (save_data inputdir) job.Builder.files in
        match db_add_build (job, uuid, console, start, finish, result, data) input_files conn with
        | Error e ->
          Logs.err (fun m -> m "Error inserting build %a: %a"
                       Fpath.pp builddir Caqti_error.pp e)
        | Ok () -> ()

let add_job conn jobdir =
  Logs.debug (fun m -> m "Adding job %a" Fpath.pp jobdir);
  match Bos.OS.Dir.contents jobdir with
  | Error (`Msg e) ->
    Logs.warn (fun m ->
        m "Error getting job %s: %s\n" (Fpath.basename jobdir) e)
  | Ok builds ->
    List.iter (add_build conn) builds

let add_jobs conn datadir =
  Bos.OS.Dir.contents datadir >>|
  List.filter (fun f -> not Fpath.(equal (v "state") f)) >>|
  List.iter (add_job conn)

let add () dbpath datadir =
  let datadir = Fpath.v datadir in
  Logs.debug (fun m -> m "Data dir: %a" Fpath.pp datadir);
  let conn =
    match Caqti_blocking.connect (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ()) with
    | Error e ->
      Logs.err (fun m -> m "Error connecting to database: %a" Caqti_error.pp e);
      exit 1
    | Ok conn ->
      conn
  in
  match add_jobs conn datadir with
  | Ok () -> ()
  | Error (`Msg e) ->
    Logs.err (fun m -> m "Error getting jobs: %s\n" e);
    exit 2

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

let user_mod action dbpath username =
  let r =
    Caqti_blocking.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_blocking.CONNECTION)  ->
    print_string "Password: ";
    flush stdout;
    (* FIXME: getpass *)
    let password = read_line () in
    let user_info = Builder_web_auth.hash ~username ~password in
    match action with
    | `Add ->
      Db.exec Builder_db.User.add user_info
    | `Update ->
      Db.exec Builder_db.User.update_user user_info
  in
  or_die 1 r

let user_add () dbpath username = user_mod `Add dbpath username

let user_update () dbpath username = user_mod `Update dbpath username

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
    Db.exec Builder_db.User.remove_user username
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
                opt non_dir_file "builder.sqlite3" &
                info ~doc ["dbpath"])

let dbpath_new =
  let doc = "sqlite3 database path" in
  Cmdliner.Arg.(value &
                opt string "builder.sqlite3" &
                info ~doc ["dbpath"])

let username =
  let doc = "username" in
  Cmdliner.Arg.(required &
                pos 0 (some string) None &
                info ~doc ~docv:"USERNAME" [])

let datadir =
  let doc = Cmdliner.Arg.info ~doc:"builder data dir" ["datadir"] in
  Cmdliner.Arg.(value &
                opt dir "/var/db/builder/" doc)

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

let add_cmd =
  let doc = "populates database with builder data" in
  let man =
    [ `S "DESCRIPTION";
      `P "Scrape builder data directory information and insert into builder-web database.";
      `P "It assumes the `full' files are stored in a directory hierarchy of the following shape:";
      `Pre "/path/to/datadir/JOB-NAME/BUILD-UUID/full";
      `P "Before parsing, The UUID in the filesystem is looked up in the database \
          to see if already exists.\
          It is assumed the UUIDs correspond.";
    ]
  in
  (Cmdliner.Term.(pure add $ setup_log $ dbpath $ datadir),
   Cmdliner.Term.info ~doc ~man "add")

let user_add_cmd =
  let doc = "add a user" in
  (Cmdliner.Term.(pure user_add $ setup_log $ dbpath $ username),
   Cmdliner.Term.info ~doc "user-add")

let user_update_cmd =
  let doc = "update a user password" in
  (Cmdliner.Term.(pure user_add $ setup_log $ dbpath $ username),
   Cmdliner.Term.info ~doc "user-update")

let user_remove_cmd =
  let doc = "remove a user" in
  (Cmdliner.Term.(pure user_remove $ setup_log $ dbpath $ username),
   Cmdliner.Term.info ~doc "user-remove")

let user_list_cmd =
  let doc = "list all users" in
  (Cmdliner.Term.(pure user_list $ setup_log $ dbpath),
   Cmdliner.Term.info ~doc "user-list")

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
    [help_cmd; add_cmd; migrate_cmd;
     user_add_cmd; user_update_cmd; user_remove_cmd; user_list_cmd]
  |> Cmdliner.Term.exit

open Rresult.R.Infix

module type CONN = Caqti_blocking.CONNECTION

let () = Mirage_crypto_rng_unix.initialize ()

let iter f xs = List.fold_left (fun r x -> r >>= fun () -> f x) (Ok ()) xs
let get_opt message = function
  | Some x -> x
  | None -> Alcotest.fail message

let or_fail x =
  match x with
  | Ok x -> x
  | Error (#Caqti_error.t as e) ->
    Alcotest.failf "DB error: %a" Caqti_error.pp e

module Testable = struct
  let builder_web_auth =
    let equal (x : _ Builder_web_auth.user_info) (y : _ Builder_web_auth.user_info) =
      x.username = y.username &&
      match x.password_hash, y.password_hash with
      | `Scrypt (hash, salt, params), `Scrypt (hash', salt', params') ->
        Cstruct.equal hash hash' &&
        Cstruct.equal salt salt' &&
        params = params'
    in
    let pp ppf { Builder_web_auth.username; password_hash } =
      match password_hash with
      | `Scrypt (hash, salt, { Builder_web_auth.scrypt_n; scrypt_r; scrypt_p }) ->
        Format.fprintf ppf "user:%s;(%d,%d,%d);%a;%a" username
          scrypt_n scrypt_r scrypt_p
          Cstruct.hexdump_pp hash Cstruct.hexdump_pp salt
    in
    Alcotest.testable
      pp
      equal

  let file =
    let equal (x : Builder_db.Rep.file) (y : Builder_db.Rep.file) =
      Fpath.equal x.filepath y.filepath &&
      Fpath.equal x.localpath y.localpath &&
      Cstruct.equal x.sha256 y.sha256 &&
      x.size = y.size
    in
    let pp ppf { Builder_db.Rep.filepath; localpath; sha256; size } =
      Format.fprintf ppf "{@[<v 1>@;<1 0>Builder_db.Rep.filepath = %a;@;<1 0>\
                          localpath = %a;@;<1 0>\
                          sha256 = %a;@;<1 0>\
                          size = %d;@;<1 0>\
                          @]@,}"
        Fpath.pp filepath Fpath.pp localpath
        Cstruct.hexdump_pp sha256 size
    in
    Alcotest.testable pp equal

  let uuid = Alcotest.testable Uuidm.pp Uuidm.equal
end

let setup_db () =
  Caqti_blocking.connect
    (Uri.make ~scheme:"sqlite3" ~path:":memory:" ~query:["create", ["true"]] ())
  >>= fun ((module Db) as conn) ->
  iter (fun migrate -> Db.exec migrate ()) Builder_db.migrate >>= fun () ->
  Ok conn

let scrypt_params = Builder_web_auth.scrypt_params
    ~scrypt_n:1024 ~scrypt_r:1 ()
let username = "test" and password = "testtest"
(* Bad, but fast *)
let auth = Builder_web_auth.hash ~scrypt_params ~username ~password ()

let add_test_user (module Db : CONN) =
  Db.exec Builder_db.User.add auth

let with_user_db f () =
  or_fail
    (setup_db () >>= fun conn ->
     add_test_user conn >>= fun () ->
     f conn)

let test_user_get_all (module Db : CONN) =
  Db.collect_list Builder_db.User.get_all () >>| fun users ->
  Alcotest.(check int) "one user" (List.length users) 1

let test_user_get_user (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >>| fun res ->
  let auth_opt = Option.map snd res in
  Alcotest.(check (option Testable.builder_web_auth)) "test user" auth_opt (Some auth)

let test_user_remove_user (module Db : CONN) =
  Db.exec Builder_db.User.remove_user username >>= fun () ->
  Db.find_opt Builder_db.User.get_user username >>| fun res ->
  let auth_opt = Option.map snd res in
  Alcotest.(check (option Testable.builder_web_auth)) "remove user" auth_opt None

let test_user_update (module Db : CONN) =
  let auth' = Builder_web_auth.hash ~scrypt_params ~username
      ~password:"differentpassword" () in
  Db.exec Builder_db.User.update_user auth' >>= fun () ->
  Db.find_opt Builder_db.User.get_user username >>| fun res ->
  let auth_opt = Option.map snd res in
  Alcotest.(check (option Testable.builder_web_auth)) "update user" auth_opt (Some auth')

let test_user_remove (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >>= function
  | None ->
    Alcotest.fail "user not found"
  | Some (id, _auth') ->
    Db.exec Builder_db.User.remove id >>= fun () ->
    Db.find_opt Builder_db.User.get_user username >>| fun res ->
    let auth_opt = Option.map snd res in
    Alcotest.(check (option Testable.builder_web_auth)) "remove user" auth_opt None

let test_user_auth (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >>| function
  | None ->
    Alcotest.fail "user not found"
  | Some (_id, auth') ->
    Alcotest.(check bool) "authorized"
      (Builder_web_auth.verify_password password auth') true

let test_user_unauth (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >>| function
  | None ->
    Alcotest.fail "user not found"
  | Some (_id, auth') ->
    Alcotest.(check bool) "unauthorized"
      (Builder_web_auth.verify_password "wrong" auth') false

let job_name = "test-job"
let script = {|#!/bin/sh
  echo '#!/bin/sh' > bin/hello.sh
  echo 'echo Hello, World!' > bin/hello.sh
  |}
let uuid = Uuidm.create `V4
let console = [(0, "Hello, World!")]
let start = Option.get (Ptime.of_float_s 0.)
let finish = Option.get (Ptime.of_float_s 1.)
let result = Builder.Exited 0
let main_binary =
  let filepath = Result.get_ok (Fpath.of_string "bin/hello.sh") in
  let localpath = Result.get_ok (Fpath.of_string "/dev/null") in
  let data = "#!/bin/sh\necho Hello, World\n" in
  let sha256 = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) in
  let size = String.length data in
  { Builder_db.Rep.filepath; localpath; sha256; size }

let add_test_build (module Db : CONN) =
  let r =
    let open Builder_db in
    Db.start () >>= fun () ->
    Db.exec Job.try_add job_name >>= fun () ->
    Db.find Job.get_id_by_name job_name >>= fun job_id ->
    Db.exec Build.add { Build.uuid; start; finish; result; console; script;
                        main_binary = None;
                        job_id } >>= fun () ->
    Db.find last_insert_rowid () >>= fun id ->
    Db.exec Build_artifact.add (main_binary, id) >>= fun () ->
    Db.find last_insert_rowid () >>= fun main_binary_id ->
    Db.exec Build.set_main_binary (id, main_binary_id) >>= fun () ->
    Db.commit ()
  in
  Rresult.R.kignore_error
    ~use:(fun _ -> Db.rollback ())
    r

let with_build_db f () =
  or_fail
    (setup_db () >>= fun conn ->
     add_test_build conn >>= fun () ->
     f conn)

let test_job_get_all (module Db : CONN) =
  Db.collect_list Builder_db.Job.get_all () >>| fun jobs ->
  Alcotest.(check int) "one job" (List.length jobs) 1

let test_job_get_id_by_name (module Db : CONN) =
  Db.find Builder_db.Job.get_id_by_name job_name >>| fun _id ->
  ()

let test_job_get (module Db : CONN) =
  Db.find Builder_db.Job.get_id_by_name job_name >>= fun job_id ->
  Db.find_opt Builder_db.Job.get job_id >>| fun job_name' ->
  Alcotest.(check (option string)) "job equal" job_name' (Some job_name)

let test_job_remove () =
  let r =
    setup_db () >>= fun (module Db : CONN) ->
    Db.exec Builder_db.Job.try_add "test-job" >>= fun () ->
    Db.find Builder_db.Job.get_id_by_name "test-job" >>= fun id ->
    Db.exec Builder_db.Job.remove id >>= fun () ->
    Db.collect_list Builder_db.Job.get_all () >>| fun jobs ->
    Alcotest.(check int) "no jobs" (List.length jobs) 0
  in
  or_fail r

let test_build_get_by_uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid
  >>| get_opt "no build" >>| fun (_id, build) ->
  Alcotest.(check Testable.uuid) "same uuid" build.uuid uuid

let test_build_get_all (module Db : CONN) =
  Db.find Builder_db.Job.get_id_by_name job_name >>= fun job_id ->
  Db.collect_list Builder_db.Build.get_all job_id >>| fun builds ->
  Alcotest.(check int) "one build" (List.length builds) 1

let test_build_get_all_meta (module Db : CONN) =
  Db.find Builder_db.Job.get_id_by_name job_name >>= fun job_id ->
  Db.collect_list Builder_db.Build.get_all_meta job_id >>| fun builds ->
  Alcotest.(check int) "one build" (List.length builds) 1

let uuid' = Uuidm.create `V4
let start' = Option.get (Ptime.of_float_s 3600.)
let finish' = Option.get (Ptime.of_float_s 3601.)

let add_second_build (module Db : CONN) =
  let uuid = uuid' and start = start' and finish = finish' in
  let open Builder_db in
  Db.start () >>= fun () ->
  Db.find Job.get_id_by_name job_name >>= fun job_id ->
  Db.exec Build.add { Build.uuid; start; finish; result; console; script;
                      main_binary = None; job_id;
                    } >>= fun () ->
  Db.find last_insert_rowid () >>= fun id ->
  Db.exec Build_artifact.add (main_binary, id) >>= fun () ->
  Db.find last_insert_rowid () >>= fun main_binary_id ->
  Db.exec Build.set_main_binary (id, main_binary_id) >>= fun () ->
  Db.commit ()

let test_build_get_latest (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  (* Test *)
  Db.find Builder_db.Job.get_id_by_name job_name >>= fun job_id ->
  Db.find_opt Builder_db.Build.get_latest job_id
  >>| get_opt "no latest build" >>| fun (_id, meta, main_binary') ->
  Alcotest.(check (option Testable.file)) "same main binary" main_binary' (Some main_binary);
  Alcotest.(check Testable.uuid) "same uuid" meta.uuid uuid'

let test_build_get_latest_uuid (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  (* Test *)
  Db.find Builder_db.Job.get_id_by_name job_name >>= fun job_id ->
  Db.find_opt Builder_db.Build.get_latest_uuid job_id
  >>| get_opt "no latest build" >>| fun (_id, latest_uuid) ->
  Alcotest.(check Testable.uuid) "same uuid" latest_uuid uuid'

let test_build_get_previous (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  Db.find_opt Builder_db.Build.get_by_uuid uuid'
  >>| get_opt "no build" >>= fun (id, _build) ->
  Db.find_opt Builder_db.Build.get_previous_successful id
  >>| get_opt "no previous build" >>| fun (_id, meta) ->
  Alcotest.(check Testable.uuid) "same uuid" meta.uuid uuid

let test_build_get_previous_none (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid
  >>| get_opt "no build" >>= fun (id, _build) ->
  Db.find_opt Builder_db.Build.get_previous_successful id >>| function
  | None -> ()
  | Some (_id, meta) ->
    Alcotest.failf "Got unexpected result %a" Uuidm.pp meta.uuid

let test_build_get_by_hash (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  Db.find_opt Builder_db.Build.get_by_hash main_binary.sha256
  >>| get_opt "no build" >>| fun (job_name', build) ->
  Alcotest.(check string) "same job" job_name' job_name;
  Alcotest.(check Testable.uuid) "same uuid" build.uuid uuid'

let test_artifact_get_all_by_build (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>|
  get_opt "no build" >>= fun (id, _build) ->
  Db.collect_list Builder_db.Build_artifact.get_all_by_build id >>| fun build_artifacts ->
  Alcotest.(check int) "one build artifact" (List.length build_artifacts) 1

let test_artifact_get_by_build_uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build_artifact.get_by_build_uuid
    (uuid, main_binary.Builder_db.Rep.filepath) >>|
  get_opt "no build" >>| fun (_id, file) ->
  Alcotest.(check Testable.file) "same file" file main_binary

let test_artifact_get_by_build (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>|
  get_opt "no build" >>= fun (id, _build) ->
  Db.find Builder_db.Build_artifact.get_by_build
    (id, main_binary.filepath)>>| fun (_id, file) ->
  Alcotest.(check Testable.file) "same file" file main_binary

(* XXX: This test should fail because main_binary on the corresponding build
 * references its main_binary. This is not the case now due to foreign key. *)
let test_artifact_remove_by_build (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>|
  get_opt "no build" >>= fun (id, _build) ->
  Db.exec Builder_db.Build_artifact.remove_by_build id

let () =
  let open Alcotest in
  Alcotest.run "Builder_db" [
    "user", [
      test_case "One user" `Quick (with_user_db test_user_get_all);
      test_case "Get user" `Quick (with_user_db test_user_get_user);
      test_case "Remove user by name" `Quick (with_user_db test_user_remove_user);
      test_case "Update user" `Quick (with_user_db test_user_update);
      test_case "Remove user" `Quick (with_user_db test_user_remove);
    ];
    "user-auth", [
      test_case "User auth success" `Quick (with_user_db test_user_auth);
      test_case "User auth fail" `Quick (with_user_db test_user_unauth);
    ];
    "job", [
      test_case "Add build" `Quick (with_build_db (fun _ -> Ok ()));
      test_case "One job" `Quick (with_build_db test_job_get_all);
      test_case "Get job id" `Quick (with_build_db test_job_get_id_by_name);
      test_case "Get job" `Quick (with_build_db test_job_get);
      test_case "Remove job" `Quick test_job_remove;
    ];
    "build", [
      test_case "Get build" `Quick (with_build_db test_build_get_by_uuid);
      test_case "One build" `Quick (with_build_db test_build_get_all);
      test_case "One build (meta data)" `Quick (with_build_db test_build_get_all_meta);
      test_case "Get latest build" `Quick (with_build_db test_build_get_latest);
      test_case "Get latest build uuid" `Quick (with_build_db test_build_get_latest_uuid);
      test_case "Get build by hash" `Quick (with_build_db test_build_get_by_hash);
      test_case "Get previous build" `Quick (with_build_db test_build_get_previous);
      test_case "Get previous build when first" `Quick (with_build_db test_build_get_previous_none);
    ];
    "build-artifact", [
      test_case "Get all by build" `Quick (with_build_db test_artifact_get_all_by_build);
      test_case "Get by build uuid" `Quick (with_build_db test_artifact_get_by_build_uuid);
      test_case "Get by build" `Quick (with_build_db test_artifact_get_by_build);
      test_case "Remove by build" `Quick (with_build_db test_artifact_remove_by_build);
    ];
  ]

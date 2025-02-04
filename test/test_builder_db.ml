let ( >>= ) = Result.bind
let ( >>| ) x f = Result.map f x

module type CONN = Caqti_blocking.CONNECTION

let () = Mirage_crypto_rng_unix.use_default ()

let iter f xs = List.fold_left (fun r x -> r >>= fun () -> f x) (Ok ()) xs
let get_opt message = function
  | Some x -> x
  | None -> Alcotest.fail message

let or_fail x =
  match x with
  | Ok x -> x
  | Error (`Msg msg) ->
    Alcotest.failf "Error: %s" msg
  | Error (#Caqti_error.t as e) ->
    Alcotest.failf "DB error: %a" Caqti_error.pp e

module Testable = struct
  let builder_web_auth =
    let equal (x : _ Builder_web_auth.user_info) (y : _ Builder_web_auth.user_info) =
      x.username = y.username &&
      x.restricted = y.restricted &&
      match x.password_hash, y.password_hash with
      | `Scrypt (hash, salt, params), `Scrypt (hash', salt', params') ->
        String.equal hash hash' &&
        String.equal salt salt' &&
        params = params'
    in
    let pp ppf { Builder_web_auth.username; password_hash; restricted } =
      match password_hash with
      | `Scrypt (hash, salt, { Builder_web_auth.scrypt_n; scrypt_r; scrypt_p }) ->
        Format.fprintf ppf "user:%s;(%d,%d,%d);%B;%a;%a" username
          scrypt_n scrypt_r scrypt_p restricted
          Ohex.pp hash Ohex.pp salt
    in
    Alcotest.testable
      pp
      equal

  let file =
    let equal (x : Builder_db.Rep.file) (y : Builder_db.Rep.file) =
      Fpath.equal x.filepath y.filepath &&
      String.equal x.sha256 y.sha256 &&
      x.size = y.size
    in
    let pp ppf { Builder_db.Rep.filepath; sha256; size } =
      Format.fprintf ppf "{@[<v 1>@;<1 0>Builder_db.Rep.filepath = %a;@;<1 0>\
                          sha256 = %a;@;<1 0>\
                          size = %d;@;<1 0>\
                          @]@,}"
        Fpath.pp filepath Ohex.pp sha256 size
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
let restricted = false
(* Bad, but fast *)
let auth = Builder_web_auth.hash ~scrypt_params ~username ~password ~restricted ()

let add_test_user (module Db : CONN) =
  Db.exec Builder_db.User.add auth >>= fun () ->
  Db.find Builder_db.last_insert_rowid ()

let with_user_db f () =
  or_fail
    (setup_db () >>= fun conn ->
     add_test_user conn >>= fun _id ->
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
      ~password:"differentpassword" ~restricted () in
  Db.exec Builder_db.User.update_user auth' >>= fun () ->
  Db.find_opt Builder_db.User.get_user username >>| fun res ->
  let auth_opt = Option.map snd res in
  Alcotest.(check (option Testable.builder_web_auth)) "update user" auth_opt (Some auth')

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
let script = Fpath.v "/dev/null"
let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) ()
let console = Fpath.v "/dev/null"
let start = Option.get (Ptime.of_float_s 0.)
let finish = Option.get (Ptime.of_float_s 1.)
let result = Builder.Exited 0
let main_binary =
  let filepath = Result.get_ok (Fpath.of_string "bin/hello.sh") in
  let data = "#!/bin/sh\necho Hello, World\n" in
  let sha256 = Digestif.SHA256.(to_raw_string (digest_string data)) in
  let size = String.length data in
  { Builder_db.Rep.filepath; sha256; size }
let main_binary2 =
  let data = "#!/bin/sh\necho Hello, World 2\n" in
  let sha256 = Digestif.SHA256.(to_raw_string (digest_string data)) in
  let size = String.length data in
  { main_binary with sha256 ; size }
let platform = "exotic-os"

let fail_if_none a =
  Option.to_result ~none:(`Msg "Failed to retrieve") a

let add_test_build user_id (module Db : CONN) =
  let open Builder_db in
  Db.start () >>= fun () ->
  Db.exec Job.try_add job_name >>= fun () ->
  Db.find_opt Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.exec Build.add { Build.uuid; start; finish; result; console; script; platform;
                      main_binary = None; input_id = None; user_id; job_id } >>= fun () ->
  Db.find last_insert_rowid () >>= fun id ->
  Db.exec Build_artifact.add (main_binary, id) >>= fun () ->
  Db.find last_insert_rowid () >>= fun main_binary_id ->
  Db.exec Build.set_main_binary (id, main_binary_id) >>= fun () ->
  Db.commit ()

let with_build_db f () =
  or_fail
    (setup_db () >>= fun conn ->
     add_test_user conn >>= fun user_id ->
     add_test_build user_id conn >>= fun () ->
     f conn)

let test_job_get_id_by_name (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name >>= fail_if_none >>| fun _id ->
  ()

let test_job_get (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.find_opt Builder_db.Job.get job_id >>| fun job_name' ->
  Alcotest.(check (option string)) "job equal" job_name' (Some job_name)

let test_job_remove () =
  let r =
    setup_db () >>= fun (module Db : CONN) ->
    Db.exec Builder_db.Job.try_add "test-job" >>= fun () ->
    Db.find_opt Builder_db.Job.get_id_by_name "test-job" >>= fail_if_none >>= fun id ->
    Db.exec Builder_db.Job.remove id >>= fun () ->
    match Db.find Builder_db.Job.get id with
    | Error #Caqti_error.call_or_retrieve -> Ok ()
    | Ok _ -> Alcotest.fail "expected no job"
  in
  or_fail r

let test_build_get_by_uuid (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid
  >>| get_opt "no build" >>| fun (_id, build) ->
  Alcotest.(check Testable.uuid) "same uuid" build.uuid uuid

let test_build_get_all (module Db : CONN) =
  Db.find_opt Builder_db.Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.collect_list Builder_db.Build.get_all job_id >>| fun builds ->
  Alcotest.(check int) "one build" (List.length builds) 1

let uuid' = Uuidm.v4_gen (Random.State.make_self_init ()) ()
let start' = Option.get (Ptime.of_float_s 3600.)
let finish' = Option.get (Ptime.of_float_s 3601.)

let add_second_build (module Db : CONN) =
  let uuid = uuid' and start = start' and finish = finish' in
  let open Builder_db in
  Db.find_opt User.get_user username >>= fail_if_none >>= fun (user_id, _) ->
  Db.start () >>= fun () ->
  Db.find_opt Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.exec Build.add { Build.uuid; start; finish; result; console; script; platform;
                     main_binary = None; input_id = None; user_id; job_id; } >>= fun () ->
  Db.find last_insert_rowid () >>= fun id ->
  Db.exec Build_artifact.add (main_binary2, id) >>= fun () ->
  Db.find last_insert_rowid () >>= fun main_binary_id ->
  Db.exec Build.set_main_binary (id, main_binary_id) >>= fun () ->
  Db.commit ()

let test_build_get_latest (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  (* Test *)
  Db.find_opt Builder_db.Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.find_opt Builder_db.Build.get_latest_successful_with_binary (job_id, platform)
  >>| get_opt "no latest build" >>| fun (_id, meta, main_binary') ->
  Alcotest.(check Testable.file) "same main binary" main_binary2 main_binary';
  Alcotest.(check Testable.uuid) "same uuid" meta.uuid uuid'

let test_build_get_previous (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  Db.find_opt Builder_db.Build.get_by_uuid uuid'
  >>| get_opt "no build" >>= fun (id, _build) ->
  Db.find_opt Builder_db.Build.get_previous_successful_different_output id
  >>| get_opt "no previous build" >>| fun build ->
  Alcotest.(check Testable.uuid) "same uuid" build.Builder_db.Build.uuid uuid

let test_build_get_previous_none (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid
  >>| get_opt "no build" >>= fun (id, _build) ->
  Db.find_opt Builder_db.Build.get_previous_successful_different_output id >>| function
  | None -> ()
  | Some build ->
    Alcotest.failf "Got unexpected result %a" Uuidm.pp build.Builder_db.Build.uuid

let test_build_get_with_jobname_by_hash (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  Db.find_opt Builder_db.Build.get_with_jobname_by_hash main_binary.sha256
  >>| get_opt "no build" >>= fun (job_name', build) ->
  Alcotest.(check string) "same job" job_name' job_name;
  Alcotest.(check Testable.uuid) "same uuid" build.uuid uuid;
  Db.find_opt Builder_db.Build.get_with_jobname_by_hash main_binary2.sha256
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

let test_artifact_exists_true (module Db : CONN) =
  Db.find Builder_db.Build_artifact.exists main_binary.sha256 >>| fun exists ->
  Alcotest.(check bool) "main binary exists" true exists

let test_artifact_exists_false (module Db : CONN) =
  Db.find Builder_db.Build_artifact.exists main_binary2.sha256 >>| fun exists ->
  Alcotest.(check bool) "main binary2 doesn't exists" false exists

(* XXX: This test should fail because main_binary on the corresponding build
 * references its main_binary. This is not the case now due to foreign key. *)
let test_artifact_remove_by_build (module Db : CONN) =
  Db.find_opt Builder_db.Build.get_by_uuid uuid >>|
  get_opt "no build" >>= fun (id, _build) ->
  Db.exec Builder_db.Build_artifact.remove_by_build id

let test_get_builds_older_than (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  let date = Option.get (Ptime.of_float_s (3600. /. 2.)) in
  Db.find_opt Builder_db.Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.collect_list Builder_db.Build.get_builds_older_than (job_id, None, date) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  Alcotest.(check (list Testable.uuid)) "last build" builds [ uuid ];
  Db.collect_list Builder_db.Build.get_builds_older_than (job_id, None, Ptime_clock.now ()) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  (* NOTE(dinosaure): from the most recent to the older. *)
  Alcotest.(check (list Testable.uuid)) "last builds" builds [ uuid'; uuid ];
  Ok ()

let test_builds_excluding_latest_n (module Db : CONN) =
  add_second_build (module Db) >>= fun () ->
  Db.find_opt Builder_db.Job.get_id_by_name job_name >>= fail_if_none >>= fun job_id ->
  Db.collect_list Builder_db.Build.get_builds_excluding_latest_n (job_id, None, 1) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  Alcotest.(check (list Testable.uuid)) "keep recent build" builds [ uuid ];
  Db.collect_list Builder_db.Build.get_builds_excluding_latest_n (job_id, None, 2) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  Alcotest.(check (list Testable.uuid)) "keep 2 builds" builds [];
  Db.collect_list Builder_db.Build.get_builds_excluding_latest_n (job_id, None, 3) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  Alcotest.(check (list Testable.uuid)) "last more builds than we have" builds [];
  Db.collect_list Builder_db.Build.get_builds_excluding_latest_n (job_id, None, 0) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  Alcotest.(check (list Testable.uuid)) "delete all builds" builds [ uuid'; uuid ];
  Db.collect_list Builder_db.Build.get_builds_excluding_latest_n (job_id, None, -1) >>= fun builds ->
  let builds = List.map (fun (_, { Builder_db.Build.uuid; _ }) -> uuid) builds in
  Alcotest.(check (list Testable.uuid)) "test an incomprehensible argument (-1)" builds [ uuid'; uuid ];
  Ok ()

let () =
  let open Alcotest in
  Alcotest.run "Builder_db" [
    "user", [
      test_case "One user" `Quick (with_user_db test_user_get_all);
      test_case "Get user" `Quick (with_user_db test_user_get_user);
      test_case "Remove user by name" `Quick (with_user_db test_user_remove_user);
      test_case "Update user" `Quick (with_user_db test_user_update);
    ];
    "user-auth", [
      test_case "User auth success" `Quick (with_user_db test_user_auth);
      test_case "User auth fail" `Quick (with_user_db test_user_unauth);
    ];
    "job", [
      test_case "Add build" `Quick (with_build_db (fun _ -> Ok ()));
      test_case "Get job id" `Quick (with_build_db test_job_get_id_by_name);
      test_case "Get job" `Quick (with_build_db test_job_get);
      test_case "Remove job" `Quick test_job_remove;
    ];
    "build", [
      test_case "Get build" `Quick (with_build_db test_build_get_by_uuid);
      test_case "One build" `Quick (with_build_db test_build_get_all);
      test_case "Get latest build" `Quick (with_build_db test_build_get_latest);
      test_case "Get build by hash" `Quick (with_build_db test_build_get_with_jobname_by_hash);
      test_case "Get previous build" `Quick (with_build_db test_build_get_previous);
      test_case "Get previous build when first" `Quick (with_build_db test_build_get_previous_none);
    ];
    "build-artifact", [
      test_case "Get all by build" `Quick (with_build_db test_artifact_get_all_by_build);
      test_case "Get by build uuid" `Quick (with_build_db test_artifact_get_by_build_uuid);
      test_case "Artifact exists" `Quick (with_build_db test_artifact_exists_true);
      test_case "Other artifact doesn't exists" `Quick (with_build_db test_artifact_exists_false);
      test_case "Remove by build" `Quick (with_build_db test_artifact_remove_by_build);
    ];
    "vacuum", [
      test_case "Get builds older than now" `Quick (with_build_db test_get_builds_older_than);
      test_case "Get older builds and keep a fixed number of then" `Quick (with_build_db test_builds_excluding_latest_n);
    ]
  ]

open Rresult.R.Infix

module type CONN = Caqti_blocking.CONNECTION

let () = Mirage_crypto_rng_unix.initialize ()

let iter f xs = List.fold_left (fun r x -> r >>= fun () -> f x) (Ok ()) xs
    
let or_fail x =
  match x with
  | Ok x -> x
  | Error (#Caqti_error.t as e) ->
    Alcotest.failf "DB error: %a" Caqti_error.pp e

let setup_db () =
  let () = print_endline "Hello, World!" in
  Caqti_blocking.connect
    (Uri.make ~scheme:"sqlite3" ~path:":memory:" ~query:["create", ["true"]] ())
  >>= fun ((module Db) as conn) ->
  iter (fun migrate -> Db.exec migrate ()) Builder_db.migrate >>= fun () ->
  Ok conn

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
  Alcotest.(check (option builder_web_auth)) "test user" auth_opt (Some auth)

let test_user_remove_user (module Db : CONN) =
  Db.exec Builder_db.User.remove_user username >>= fun () ->
  Db.find_opt Builder_db.User.get_user username >>| fun res ->
  let auth_opt = Option.map snd res in
  Alcotest.(check (option builder_web_auth)) "remove user" auth_opt None

let test_user_update (module Db : CONN) =
  let auth' = Builder_web_auth.hash ~scrypt_params ~username
      ~password:"differentpassword" () in
  Db.exec Builder_db.User.update_user auth' >>= fun () ->
  Db.find_opt Builder_db.User.get_user username >>| fun res ->
  let auth_opt = Option.map snd res in
  Alcotest.(check (option builder_web_auth)) "update user" auth_opt (Some auth')

let test_user_remove (module Db : CONN) =
  Db.find_opt Builder_db.User.get_user username >>= function
  | None ->
    Alcotest.fail "user not found"
  | Some (id, _auth') ->
    Db.exec Builder_db.User.remove id >>= fun () ->
    Db.find_opt Builder_db.User.get_user username >>| fun res ->
    let auth_opt = Option.map snd res in
    Alcotest.(check (option builder_web_auth)) "remove user" auth_opt None

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
  ]

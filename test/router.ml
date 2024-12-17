
module Param_verification = struct

  (*> None is 'verified'*)
  type t = wrong_type option
  [@@deriving yojson,show,eq]

  and wrong_type = {
    param : string;
    expected : string;
  }

  let alcotyp = Alcotest.testable pp equal

  module P = struct

    let is_string : (string * string) -> _ option  =
      Fun.const None

    let is_uuid (param, value) =
      match Uuidm.of_string value with
      | Some _ when String.length value = 36 -> None
      | _ -> Some {
          param;
          expected = "Uuidm.t"
        }

  end

  let verify parameters req =
    let verified_params =
      List.fold_left (fun acc p ->
        match acc with
        | None ->
          if String.starts_with ~prefix:"build" p then
           P.is_uuid (p, Dream.param req p)
          else
            P.is_string (p, Dream.param req p)
        | Some _ as x -> x)
      None parameters
    in
    let response_json =
      verified_params |> to_yojson |> Yojson.Safe.to_string
    in
    Dream.respond response_json

end

let find_parameters path =
  List.filter_map (fun s ->
      if String.length s > 0 && String.get s 0 = ':' then
        Some (String.sub s 1 (String.length s - 1))
      else
        None)
    (String.split_on_char '/' path)

let router =
  (* XXX: this relies on [Builder_web.routes] only using {data,cache,config}dir
   * in the handlers which are never called here. The path /nonexistant is
   * assumed to not exist. *)
  let nodir = Fpath.v "/nonexistant" in
  Builder_web.routes ~datadir:nodir ~cachedir:nodir ~configdir:nodir ~expired_jobs:0
  |> List.map (fun (meth, route, _handler) ->
      meth, route, Param_verification.verify (find_parameters route))
  |> Builder_web.to_dream_routes
  |> Dream.router
  (* XXX: we test without remove_trailing_url_slash to ensure we don't produce
   * urls with trailing slashes: *)
  (* |> Builder_web.Middleware.remove_trailing_url_slash *)
  |> Dream.test

let test_link method_ target () =
  let req = Dream.request ~method_ ~target "" in
  let resp = router req in
  let status_code = Dream.(status resp |> status_to_int) in
  Alcotest.(check' int ~msg:"status-code" ~actual:status_code ~expected:200);
  let body =
    Dream.body resp
    |> Lwt_main.run
    |> Yojson.Safe.from_string
    |> Param_verification.of_yojson
  in
  Alcotest.(check' (result Param_verification.alcotyp string) ~msg:"param-verification"
              ~actual:body ~expected:(Ok None))

let test_link_artifact artifact =
  let job_name = "test" in
  let build = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  test_link `GET @@
  Builder_web.Link.Job_build_artifact.make ~job_name ~build ~artifact ()

let () =
  Alcotest.run "Router" [
    "Link module synced", Alcotest.[
        test_case "Link.Root.make" `Quick begin
          test_link `GET @@ Builder_web.Link.Root.make ()
        end;
        test_case "Link.Job.make" `Quick begin
          let queries = [ `Platform "test" ] in
          let job_name = "test" in
          test_link `GET @@ Builder_web.Link.Job.make ~queries ~job_name ()
        end;
        test_case "Link.Job.make_failed" `Quick begin
          let queries = [ `Platform "test" ] in
          let job_name = "test" in
          test_link `GET @@
          Builder_web.Link.Job.make_failed ~queries ~job_name ()
        end;
        test_case "Link.Job_build.make" `Quick begin
          let job_name = "test" in
          let build = Uuidm.v4_gen (Random.State.make_self_init ()) () in
          test_link `GET @@ Builder_web.Link.Job_build.make ~job_name ~build ()
        end;
        test_case "Link.Job_build_artifact.make_from_string" `Quick begin
          let job_name = "test" in
          let build = Uuidm.v4_gen (Random.State.make_self_init ()) () in
          let artifact = "" in
          test_link `GET @@
          Builder_web.Link.Job_build_artifact.make_from_string
            ~job_name ~build ~artifact ()
        end;
      ] @ Alcotest.(
        [
          `Main_binary;
          `Viz_treemap;
          `Viz_dependencies;
          `Script;
          `Console;
          `All_targz;
          `File Fpath.(v "some" / "path");
        ]
        |> List.map (fun artifact ->
            let descr =
              Fmt.str "Job_build_artifact.make: %s" @@
              Builder_web.Link.Job_build_artifact.encode_artifact artifact
            in
            test_case descr `Quick begin
              test_link_artifact artifact
            end
          )
      ) @ Alcotest.[
        test_case "Link.Compare_builds.make" `Quick begin
          let left = Uuidm.v4_gen (Random.State.make_self_init ()) () in
          let right = Uuidm.v4_gen (Random.State.make_self_init ()) () in
          test_link `GET @@
          Builder_web.Link.Compare_builds.make ~left ~right ()
        end;
        test_case "Link.Failed_builds.make" `Quick begin
          test_link `GET @@
          Builder_web.Link.Failed_builds.make ~count:2 ~start:1 ()
        end;
      ];
    (* this doesn't actually test the redirects, unfortunately *)
    "Latest", List.map (fun p -> Alcotest.(test_case ("…"^p) `Quick begin
        test_link `GET @@ "/job/test/build/latest" ^ p end))
      [
        "/f/bin/unikernel.hvt";
        "/";
        "";
      ];
    "Albatross hardcoded links",
    [
      (*> Note: these links can be found in
          albatross/command-line/albatross_client_update.ml
          .. to find them I follewed the trails of 'Albatross_cli.http_host'
      *)
      begin
        let sha_str =
          Digestif.SHA256.(to_raw_string (digest_string "foo"))
          |> Ohex.encode
        in
        Fmt.str "/hash?sha256=%s" sha_str
      end;
      begin
        let jobname = "foo" in
        "/job/" ^ jobname ^ "/build/latest"
      end;
      begin
        let job = "foo" in
        let build = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
        "/job/" ^ job ^ "/build/" ^ build ^ "/main-binary"
      end;
      begin
        let old_uuid = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
        let new_uuid = Uuidm.(v4_gen (Random.State.make_self_init ()) () |> to_string) in
        Fmt.str "/compare/%s/%s" old_uuid new_uuid
      end;
    ]
    |> List.map Alcotest.(fun p ->
        test_case ("…" ^ p) `Quick (test_link `GET p))
  ]

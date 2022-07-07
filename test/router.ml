
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

    let is_string : (string * string) option -> _ option  = function
      | Some _ -> None
      | None -> None

    let is_uuid = function
      | Some (param, value) ->
        begin match Uuidm.of_string value with
          | Some _ -> None
          | None -> Some {
              param;
              expected = "Uuidm.t"
            }
        end
      | None -> None
    
  end
  
  let param req tag =
    try Some (tag, Dream.param req tag) with _ -> None

  let ( &&& ) v v' =
    match v with
    | None -> v'
    | Some _ as some -> some

  let verify req =
    let verified_params = 
          (param req "job" |> P.is_string)
      &&& (param req "build" |> P.is_uuid)
      &&& (param req "build_left" |> P.is_uuid)
      &&& (param req "build_right" |> P.is_uuid)
      &&& (param req "platform" |> P.is_string)
    in
    let response_json =
      verified_params |> to_yojson |> Yojson.Safe.to_string
    in
    Dream.respond response_json

end
                   
let router =
  let tmp = Fpath.v "/tmp" in
  Builder_web.routes ~datadir:tmp ~cachedir:tmp ~configdir:tmp
  |> List.map (fun (meth, route, _handler) ->
      meth, route, Param_verification.verify)
  |> Builder_web.to_dream_routes
  |> Dream.router
  |> Builder_web.Middleware.remove_trailing_url_slash
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
    |> Result.get_ok
  in
  Alcotest.(check' Param_verification.alcotyp ~msg:"param-verification"
              ~actual:body ~expected:None)

let test_link_artifact artifact = 
  let job_name = "test" in
  let build = Uuidm.v `V4 in
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
          let build = Uuidm.v `V4 in
          test_link `GET @@ Builder_web.Link.Job_build.make ~job_name ~build ()
        end;
        test_case "Link.Job_build_artifact.make_from_string" `Quick begin
          let job_name = "test" in
          let build = Uuidm.v `V4 in
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
          let left = Uuidm.v `V4 in
          let right = Uuidm.v `V4 in
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
      ]
  ]

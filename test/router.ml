type route = Route : [`GET|`POST] * ('e, 'f, unit) Vif.Uri.t * 'f -> route

let route meth path =
  let fin = ()
  and string k (_job : string) = k
  and uuid k (_build : Uuidm.t) = k
  and script_or_console k (_ : [`Script|`Console]) = k
  and viz k (_ : [`Dependencies|`Treemap]) = k
  and ( @-> ) v k = v k
  and get uri fn = Route (`GET, uri, fn)
  and post uri fn = Route (`POST, uri, fn)
  and ( $ ) f x = f x
  in
  let routes = [
    (* XXX: this must stay in sync with Builder_web.routes *)
    get $ Url.root () $ fin;
    get $ Url.all_builds () $ fin;
    get $ Url.failed_builds () $ fin;
    get $ Url.job () $ string @-> fin;
    get $ Url.job_with_failed () $ string @-> fin;
    get $ Url.redirect_latest () $ string @-> string @-> fin;
    get $ Url.redirect_latest_empty () $ string @-> fin;
    get $ Url.job_build () $ string @-> uuid @-> fin;
    get $ Url.job_build_file () $ string @-> uuid @-> string @-> fin;
    get $ Url.job_build_static_file () $ string @-> uuid @-> script_or_console @-> fin;
    get $ Url.job_build_viz () $ string @-> uuid @-> viz @-> fin;
    get $ Url.exec () $ string @-> uuid @-> fin;
    get $ Url.redirect_main_binary () $ string @-> uuid @-> fin;
    get $ Url.compare_builds () $ uuid @-> uuid @-> fin;
    get $ Url.hash () $ string @-> fin;
    get $ Url.robots () $ fin;
    post $ Url.upload () $ fin;
    post $ Url.upload_binary () $ string @-> string @-> fin;
  ] in
  List.exists
    (fun (Route (meth', uri, fn)) ->
       meth = meth' &&
       match Vif.Uri.extract uri path fn with
       | Ok () -> true
       | Error `Converter_failure exn ->
         Alcotest.failf "Converter raised: %a" Fmt.exn exn
       | Error _ -> false)
    routes

let route_test meth path =
  path, `Quick,
  fun () ->
  if route meth path then
    ()
  else
    Alcotest.failf "Did not route %s" path

let link_module_synced =
  let module Link = Builder_web.Link in
  [
    begin
      route_test `GET @@ Builder_web.Link.Root.make ()
    end;
    begin
      let queries = [ `Platform "test" ] in
      let job_name = "test" in
      route_test `GET @@ Builder_web.Link.Job.make ~queries ~job_name ()
    end;
    begin
      let queries = [ `Platform "test" ] in
      let job_name = "test" in
      route_test `GET @@
      Builder_web.Link.Job.make_failed ~queries ~job_name ()
    end;
    begin
      let job_name = "test" in
      let build = Uuidm.v4_gen (Random.State.make_self_init ()) () in
      route_test `GET @@ Builder_web.Link.Job_build.make ~job_name ~build ()
    end;
    begin
      let job_name = "test" in
      let build = Uuidm.v4_gen (Random.State.make_self_init ()) () in
      let artifact = "" in
      route_test `GET @@
      Builder_web.Link.Job_build_artifact.make_from_string
        ~job_name ~build ~artifact ()
    end;
  ] @
  List.map
    (fun artifact ->
       let build = Uuidm.of_string "08455214-8a9b-413a-b116-a65353dc0743" |> Option.get in
       route_test `GET @@
       Link.Job_build_artifact.make ~job_name:"test" ~build ~artifact ())
    [
      `Main_binary;
      `Viz_treemap;
      `Viz_dependencies;
      `Script;
      `Console;
      `File Fpath.(v "some" / "path");
    ]
  @
  [
  (let left = Uuidm.of_string "08455214-8a9b-413a-b116-a65353dc0743" |> Option.get
   and right = Uuidm.of_string "f63a5b8a-9ab3-40a3-8cfc-fcbc1dc68df9" |> Option.get in
   route_test `GET @@
   Link.Compare_builds.make ~left ~right ());
  (route_test `GET @@
   Link.Failed_builds.make ~count:2 ~start:1 ());
]

(* this doesn't actually test the redirects, unfortunately *)
let latest =
  List.map
    (fun path ->
       route_test `GET ("/job/test/build/latest" ^ path))
    [
      "/f/bin/unikernel.hvt";
      "/";
      "";
    ]

let foo_sha256 =
  Digestif.SHA256.(to_raw_string (digest_string "foo"))
  |> Ohex.encode

let albatross = [
  (*> Note: these links can be found in
      albatross/command-line/albatross_client_update.ml
      .. to find them I followed the trails of 'Albatross_cli.http_host' *)
  route_test `GET (Fmt.str "/hash?sha256=%s" foo_sha256);
  route_test `GET "/job/foo/build/latest";
  route_test `GET "/compare/08455214-8a9b-413a-b116-a65353dc0743/f63a5b8a-9ab3-40a3-8cfc-fcbc1dc68df9";
]

let () =
  Alcotest.run "Router" [
    "Link module synced", link_module_synced;
    "Latest", latest;
    "Albatross_hardcoded_links", albatross;
  ]

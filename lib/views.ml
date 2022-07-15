module H = Tyxml.Html

let pp_ptime ppf ptime =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime in
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02dZ" y m d hh mm ss

let txtf fmt = Fmt.kstr H.txt fmt
let a_titlef fmt = Fmt.kstr H.a_title fmt

let check_icon result =
  match result with
  | Builder.Exited 0 ->
    H.span ~a:H.[
        a_style "color: green; cursor: pointer;";
        a_titlef "%a" Builder.pp_execution_result result;
      ]
      [H.txt "☑"]
  | _ ->
    H.span ~a:H.[
        a_style "color: red; cursor: pointer;";
        a_titlef "%a" Builder.pp_execution_result result;
      ]
      [H.txt "☒"]

type nav = [
  | `Default
  | `Job of string * string option
  | `Build of string * Builder_db.Build.t
  | `Comparison of (string * Builder_db.Build.t) * (string * Builder_db.Build.t)
]

let pp_platform =
  Fmt.(option ~none:(any "") (append (any "on ") string))

let static_css = Tyxml.Html.Unsafe.data {|
body {
  margin: 40px auto;
  line-height: 1.6;
  color: #444;
  background: rgb(200,200,200);
  padding: 0 10px;
}
nav ul {
  display: flex;
  list-style: none;
}
nav ul li::before {
  content: "→";
}
nav ul li:first-child::before {
  content: "";
}
nav a {
  padding: .5em 1em;
}
h1,h2,h3{line-height:1.2}
.output-ts {
  white-space: nowrap;
  cursor: pointer;
  user-select: none;
}
.output-ts a {text-decoration: none;}
.output-ts a:hover {text-decoration: underline;}
.output-code {
  overflow: visible;
  white-space: pre;
}
.toggleable {
  display: none;
}
.toggleable-descr {
  cursor: pointer;
  text-decoration: underline;
  user-select: none;
}
:checked + .toggleable {
  display: block;
}
|}

let make_breadcrumbs nav =
  let to_nav kvs =
    H.nav [
      H.ul (
        List.map (fun (desc, href) ->
            H.li [H.a ~a:H.[a_href href] [desc]]
          ) kvs
      )]
  in
  match nav with
  | `Default ->
    to_nav [H.txt "Home", "/"]
  | `Job (job_name, platform) ->
    let queries =
      platform |> Option.map (fun v -> `Platform v) |> Option.to_list in
    to_nav [
      H.txt "Home", "/";
      txtf "Job %s" job_name, Link.Job.make ~job_name ();
      (
        txtf "%a" pp_platform platform,
        Link.Job.make ~job_name ~queries () 
      )
    ]
  | `Build (job_name, build) ->
    to_nav [
      H.txt "Home", "/";
      txtf "Job %s" job_name, Link.Job.make ~job_name ();
      (
        txtf "%a" pp_platform (Some build.Builder_db.Build.platform),
        Link.Job.make ~job_name
          ~queries:[ `Platform build.Builder_db.Build.platform ] ()
      );
      (
        txtf "Build %a" pp_ptime build.Builder_db.Build.start,
        Link.Job_build.make ~job_name ~build:build.Builder_db.Build.uuid ()
      );
    ]
  | `Comparison ((job_left, build_left), (job_right, build_right)) ->
    to_nav [
      H.txt "Home", "/";
      (
        txtf "Comparison between %s@%a and %s@%a"
          job_left pp_ptime build_left.Builder_db.Build.start
          job_right pp_ptime build_right.Builder_db.Build.start,
        Link.Compare_builds.make 
          ~left:build_left.uuid
          ~right:build_right.uuid ()
      );
    ]

let layout
    ?include_static_css
    ?(nav=`Default)
    ?(manual_width=false)
    ~title
    body
  =
  let breadcrumb = make_breadcrumbs nav in
  (*> Note: Last declared CSS wins - so one can override here*)
  let static_css = static_css :: Option.to_list include_static_css
  in
  let body =
    let style_grid_container = H.a_style "\
      display: flex;
      align-items: center;
      justify-content: center;
      min-width: 83em;
    "
    and style_grid = H.a_style @@
      if manual_width then "" else "\
        width: 76%;\
      "
    in
    [ H.div ~a:[ style_grid_container ]
        [ H.div ~a:[ style_grid ] body ]]
  in
  H.html
    (H.head (H.title (H.txt title))
       [H.style ~a:H.[a_mime_type "text/css"] static_css])
    (H.body [
        breadcrumb;
        H.main body
      ])

let toggleable ?(hidden=true) ~id ~description content =
  let checked = if hidden then [] else H.[a_checked ()] in
  H.div [
    H.label
      ~a:H.[
          a_label_for id;
          a_class ["toggleable-descr"];
        ]
      [H.txt description];
    H.input
      ~a:(checked @ H.[
          a_input_type `Checkbox;
          a_id id;
          a_style "display: none;";
        ]) ();
    H.div
      ~a:H.[
          a_class ["toggleable"]
        ]
      content;
  ]

let artifact
    ~basename
    ~job_name
    ~build
    ~file:{ Builder_db.filepath; localpath = _; sha256; size }
  =
  let artifact_link =
    Link.Job_build_artifact.make
      ~job_name
      ~build:build.Builder_db.Build.uuid
      ~artifact:(`File filepath) ()
  in
  [
    H.a ~a:H.[a_href artifact_link] [
      if basename then H.txt (Fpath.basename filepath)
      else txtf "%a" Fpath.pp filepath
    ];
    H.txt " ";
    H.code [txtf "SHA256:%a" Hex.pp (Hex.of_cstruct sha256)];
    txtf " (%a)" Fmt.byte_size size;
  ]

let page_not_found ~target ~referer =
  [
    H.h2 ~a:[ H.a_style "padding-top: 33vh" ]
      [ txtf "This page does not exist" ];
    H.p [
      H.txt @@ Fmt.str "You requested the page %s" target
    ];
  ] @ (
    match referer with
    | None -> []
    | Some prev_url -> [
        H.p [
          H.txt "Go back to "; 
          H.a ~a:H.[ a_href prev_url ] [ H.txt prev_url ];
        ];
      ]
  )
  |> layout ~title:"Page not found"

let viz_not_found =
  let title = "Visualization not found" in
  let content =
    [
      H.h2 ~a:[ H.a_style "\
        padding-top: 41vh;\
        text-align: center;\
      "]
        [ txtf "%s" title ];
    ]
  in
  let static_css = static_css :: [ Tyxml.Html.Unsafe.data "\
    body { background: rgb(191,191,191); }\
  "]
  in
  let body = [ H.div content ] in
  H.html
    (H.head (H.title (H.txt title))
       [H.style ~a:H.[a_mime_type "text/css"] static_css])
    (H.body [
        H.main body
      ])


module Builds = struct

  let data =
    {|
# Reproducible OPAM builds

This website offers binary MirageOS unikernels and supplementary OS packages.
If you want to use our binary packages and setup unikernels, follow
[these instructions](https://robur.coop/Projects/Reproducible_builds).
The unikernels are statically linked executables where the execution target is
independent of the build platform - so even if they're compiled on a FreeBSD
system they can be run on a Linux or OpenBSD host. Many are executed using a
[solo5](https://github.com/solo5/solo5) tender.
The filename suffix of the unikernel binary indicate the expected execution environment:
- `.hvt`: hardware virtualized - requires `solo5-hvt`
  ([Linux KVM](https://www.linux-kvm.org/page/Main_Page),
   [FreeBSD BHyve](https://wiki.freebsd.org/bhyve),
   [OpenBSD VMM](https://man.openbsd.org/vmm)),
- `.spt`: sandboxed process - requires `solo5-spt` (Linux with seccomp),
- `.xen`:  Xen PVH virtual machine (on a Xen or QubesOS host),
- `.virtio`: any virtio environment (qemu, GCE, KVM, BHyve),
- `.muen`: on [muen](https://muen.sk).

A persistent link to the latest successful build is available as
`/job/*jobname*/build/latest/`. Each build can be reproduced with
[orb](https://github.com/roburio/orb/). The builds are scheduled and executed
daily by [builder](https://github.com/roburio/builder/). This web interface is
[builder-web](https://git.robur.io/robur/builder-web/). Read further information
[on our project page](https://robur.coop/Projects/Reproducible_builds). This
work has been funded by the European Union under the
[NGI Pointer](https://pointer.ngi.eu) program. Contact team ATrobur.coop if you
have questions or suggestions.
|}

  let make_header =
    [
      H.Unsafe.data (Utils.Omd.html_of_string data);
      H.form ~a:H.[a_action "/hash"; a_method `Get] [
        H.label [
          H.txt "Search artifact by SHA256";
          H.br ();
          H.input ~a:H.[
              a_input_type `Search;
              a_id "sha256";
              a_name "sha256";
            ] ();
        ];
        H.input ~a:H.[
            a_input_type `Submit;
            a_value "Search";
          ] ();
      ];
    ]

  let make_platform_builds ~job_name (platform, latest_build, latest_artifact) =
    [
      check_icon latest_build.Builder_db.Build.result;
      H.txt " ";
      H.a ~a:[
        H.a_href @@ Link.Job.make ~job_name
          ~queries:[ `Platform platform ] ()
      ]
        [H.txt platform];
      H.txt " ";
      H.a ~a:[
        H.a_href @@ Link.Job_build.make
          ~job_name
          ~build:latest_build.Builder_db.Build.uuid ()]
        [txtf "%a" pp_ptime latest_build.Builder_db.Build.start];
      H.txt " ";
    ]
    @ (match latest_artifact with
        | Some main_binary ->
          artifact
            ~basename:true
            ~job_name
            ~build:latest_build
            ~file:main_binary
        | None ->
          [ txtf "Build failure: %a" Builder.pp_execution_result
              latest_build.Builder_db.Build.result ]
      )
    @ [ H.br () ]

  let make_jobs jobs =
    jobs |> List.map (fun (job_name, synopsis, platform_builds) ->
        H.li (
          [
            H.a ~a:H.[a_href ("/job/" ^ job_name ^ "/")]
              [H.txt job_name];
            H.br ();
            H.txt (Option.value ~default:"" synopsis);
            H.br ()
          ]
          @ List.concat_map (make_platform_builds ~job_name) platform_builds
        )
      )

  let make_body section_job_map =
    let aux  section jobs acc =
      acc @ [
        H.h2 [ H.txt section ];
        H.ul (make_jobs jobs)
      ]
    in
    Utils.String_map.fold aux section_job_map []

  let make_failed_builds =
    [ H.p [
          H.txt "View the latest failed builds ";
          H.a ~a:H.[a_href "/failed-builds"]
            [H.txt "here"];
          H.txt "."
        ]]

  let make section_job_map =
    layout ~title:"Reproducible OPAM builds"
      (make_header
       @ make_body section_job_map
       @ make_failed_builds)

end

module Job = struct

  let make_header ~job_name ~platform ~readme =
    H.h1 [txtf "Job %s %a" job_name pp_platform platform]
    :: (
      match readme with
      | None -> []
      | Some data ->
        [
          H.h2 ~a:H.[a_id "readme"] [H.txt "README"];
          H.a ~a:H.[a_href "#builds"] [H.txt "Skip to builds"];
          H.Unsafe.data (Utils.Omd.html_of_string data)
        ]
    )

  let make_build ~job_name (build, main_binary) =
    H.li (
      [
        check_icon build.Builder_db.Build.result;
        txtf " %s " build.platform;
        H.a ~a:H.[
            a_href @@ Link.Job_build.make 
              ~job_name
              ~build:build.Builder_db.Build.uuid () ]
          [
            txtf "%a" pp_ptime build.Builder_db.Build.start;
          ];
        H.txt " ";
      ]
      @ match main_binary with
      | Some main_binary ->
        artifact
          ~basename:true
          ~job_name
          ~build
          ~file:main_binary
      | None ->
        [ txtf "Build failure: %a" Builder.pp_execution_result
            build.Builder_db.Build.result ]
    )

  let make_builds ~failed ~job_name ~platform builds =
    [
      H.h2 ~a:H.[a_id "builds"] [H.txt "Builds"];
      H.a ~a:H.[a_href "#readme"] [H.txt "Back to readme"];
      H.ul (builds |> List.map (make_build ~job_name));
      let queries =
        platform |> Option.map (fun p -> `Platform p) |> Option.to_list
      in
      if failed then
        H.p [
          H.txt "Excluding failed builds " ;
          H.a ~a:H.[
              a_href @@ Link.Job.make ~job_name ~queries ()
            ]
            [H.txt "here"] ;
          H.txt "." ]
      else
        H.p [
          H.txt "Including failed builds " ; 
          H.a ~a:H.[
              a_href @@ Link.Job.make_failed ~job_name ~queries ()
            ]
            [H.txt "here"] ;
          H.txt "." ]
    ]

  let make_body ~failed ~job_name ~platform ~readme builds =
    make_header ~job_name ~platform ~readme
    @ make_builds ~failed ~job_name ~platform builds

  let make ~failed ~job_name ~platform ~readme builds =
    let nav = `Job (job_name, platform) in
    let title = Fmt.str "Job %s %a" job_name pp_platform platform in
    layout ~nav ~title @@ make_body ~failed ~job_name ~platform ~readme builds


end

module Job_build = struct

  let contains_debug_bin artifacts =
    let check f =
      Fpath.has_ext "debug" f.Builder_db.filepath
    in
    List.exists check artifacts

  let make_artifacts
      ~job_name
      ~build_uuid
      ~artifacts
      ~main_binary
      ~solo5_manifest =
    let solo5_devices solo5_manifest =
      let pp_devices =
        let pp_device_name ppf = function
          | Solo5_elftool.Dev_block_basic name | Solo5_elftool.Dev_net_basic name ->
            Fmt.pf ppf "%S" name
        in
        Fmt.(list ~sep:(any ", ") pp_device_name)
      in
      match
        List.partition (function Solo5_elftool.Dev_block_basic _ -> true | _ -> false)
          solo5_manifest.Solo5_elftool.entries
      with
      | [], [] -> [txtf "with no devices in solo5 manifest"]
      | (_::_) as block_devices, [] ->
        [txtf "with block devices %a" pp_devices block_devices]
      | [], ((_::_) as net_devices) ->
        [txtf "with net devices %a" pp_devices net_devices]
      | block_devices, net_devices ->
        [txtf "with block devices %a, and net devices %a"
           pp_devices block_devices pp_devices net_devices]
    in
    let aux (file:Builder_db.file) =
      let (`Hex sha256_hex) = Hex.of_cstruct file.sha256 in
      [
        H.dt [
          H.a ~a:H.[a_href @@ Link.Job_build_artifact.make
                      ~job_name
                      ~build:build_uuid
                      ~artifact:(`File file.filepath) ()
                   ]
            [H.code [txtf "%a" Fpath.pp file.filepath]] ];
        H.dd ([
            H.code [H.txt "SHA256:"; H.txt sha256_hex];
            txtf " (%a)" Fmt.byte_size file.size;
          ] @
            match main_binary, solo5_manifest with
            | Some main_binary, Some solo5_manifest when main_binary = file ->
              (H.br () :: solo5_devices solo5_manifest)
            | _ -> []);
      ]
    in
    [
      H.h3 [H.txt "Build artifacts"];
      H.dl (List.concat_map aux artifacts)
    ]

  let make_reproductions
      ~job_name
      ~(build:Builder_db.Build.t)
      ~same_input_same_output
      ~different_input_same_output
    =
    let same_input_same_output_html =
      List.map (fun (build:Builder_db.Build.t) ->
          H.li [
            txtf "on %s, same input, " build.platform;
            H.a ~a:H.[a_href @@ Link.Job_build.make ~job_name ~build:build.uuid ()]
              [txtf "%a" pp_ptime build.start]
          ])
        same_input_same_output
    in
    let different_input_same_output_html =
      List.map (fun (build':Builder_db.Build.t) ->
          H.li [
            txtf "on %s, different input, " build'.platform;
            H.a ~a:H.[
                a_href @@ Link.Compare_builds.make
                  ~left:build'.uuid
                  ~right:build.uuid ()]
              [txtf "%a" pp_ptime build'.start]
          ])
        different_input_same_output
    in
    [
      H.h3 [
        txtf "Reproduced by %d builds"
          (List.length (same_input_same_output @ different_input_same_output))] ;
      H.ul @@ (
        same_input_same_output_html
        @ different_input_same_output_html
      )
    ]

  let make_not_reproducible
      ~(build:Builder_db.Build.t)
      ~same_input_different_output
    =
    if same_input_different_output = [] then
      []
    else
      [ H.h3 [H.txt "Same input, different output (not reproducible!)"];
        H.ul (
          List.map (fun (build':Builder_db.Build.t) ->
              H.li [
                txtf "on %s, " build'.platform ;
                H.a ~a:H.[
                    a_href @@ Link.Compare_builds.make
                      ~left:build'.uuid
                      ~right:build.uuid ()]
                  [txtf "%a" pp_ptime build'.start]
              ])
            same_input_different_output)
      ]

  let make_comparisons_same_platform
      ~(build:Builder_db.Build.t)
      ~previous
      ~latest
      ~next
    =
    [
      H.h3 [H.txt "Comparisons with other builds on the same platform"];
      let opt_build (ctx, build') =
        match build' with
        | Some b when not (Uuidm.equal build.uuid b.Builder_db.Build.uuid) ->
          [ H.li [ H.txt ctx;
                   H.a ~a:[
                     H.a_href @@ Link.Compare_builds.make 
                       ~left:b.uuid
                       ~right:build.uuid () ]
                     [txtf "%a" pp_ptime b.start]]
          ]
        | _ -> []
      in
      H.ul
        (List.concat_map opt_build
           [ ("Latest build ", latest) ;
             ("Later build with different output ", next) ;
             ("Earlier build with different output ", previous) ])
    ]

  let make_build_info
      ~job_name
      ~delta
      ~(build:Builder_db.Build.t)
      ~artifacts
      ~main_binary
      ~solo5_manifest
      ~same_input_same_output
      ~different_input_same_output
      ~same_input_different_output
      ~latest ~next ~previous
    =
    [
      H.h2 ~a:H.[a_id "build"] [txtf "Build %a" pp_ptime build.start];
      H.p [txtf "Built on platform %s" build.platform ];
      H.p [txtf "Build took %a." Ptime.Span.pp delta ];
      H.p [txtf "Execution result: %a." Builder.pp_execution_result build.result];
      H.h3 [H.txt "Build info"];
      H.ul [
        H.li [
          H.a ~a:H.[
              a_href @@ Link.Job_build_artifact.make
                ~job_name
                ~build:build.uuid
                ~artifact:`Console ()
            ] [H.txt "Console output"];
        ];
        H.li [
          H.a ~a:H.[
              a_href @@ Link.Job_build_artifact.make
                ~job_name
                ~build:build.uuid
                ~artifact:`Script ()
            ] [H.txt "Build script"];
        ]
      ];
    ]
    @ make_artifacts
      ~job_name
      ~build_uuid:build.uuid
      ~artifacts
      ~main_binary
      ~solo5_manifest
    @ make_reproductions
      ~job_name
      ~build
      ~same_input_same_output
      ~different_input_same_output
    @ make_not_reproducible ~build ~same_input_different_output
    @ make_comparisons_same_platform
      ~build
      ~previous
      ~latest
      ~next

  let viz_style_deps = "
      border: 0;
      width: 45em;
      height: 45.4em;
      max-width: 100%;
      max-height: 49vw;
      min-width: 38em;
      min-height: 40em;
    "

  let viz_style_treemap = "
      border: 0;
      width: 46em;
      height: 49.4em;
      max-width: 100%;
      max-height: 52vw;
      min-width: 38em;
      min-height: 43em;
    "

  let make_description descr_txt =
    H.span [ H.txt "?" ] ~a:H.[
        a_title descr_txt;
        a_style "\
          font-size: 1.2em;\
          font-weight: bold;\
          "
      ]
  
  let make_viz_section ~job_name ~artifacts ~uuid =
    let viz_deps = 
      let iframe = 
        let src = Link.Job_build_artifact.make ~job_name ~build:uuid
            ~artifact:`Viz_dependencies () in
        H.iframe ~a:H.[
            a_src src;
            a_title "Opam dependencies";
            a_style viz_style_deps
          ] []
      in
      let descr_txt = "\
This is an interactive visualization of dependencies, \
focusing on how shared dependencies are. 

In the middle you see the primary package. \
Edges shoot out to its direct \
dependencies, including build dependencies. 

From these direct dependencies, edges shoot out to sets \
of their own respective direct dependencies. \
These dependency-sets include duplicates (i.e. shared dependencies) \
across the other dependency sets \
- which are shown by hovering over the \
direct dependencies of the primary package.

The lightness of nodes correspond to how shared they are. See \
the exact amount of reverse dependencies in the tooltip for each \
dependency.\
"
      in
      [ iframe; H.br (); make_description descr_txt ]
    in
    let viz_treemap = lazy (
      let iframe = 
        let src = Link.Job_build_artifact.make ~job_name ~build:uuid
            ~artifact:`Viz_treemap () in
        H.iframe ~a:H.[
            a_src src;
            a_title "Binary dissection";
            a_style viz_style_treemap
          ] []
      in
      let descr_txt = "\
This interactive treemap shows the space-usage of modules/libraries inside the \
ELF binary. You can get more info from each block by \
hovering over them. 

On top of the treemap there is a scale, showing how much space the \
treemap itself constitutes of the binary, the excluded symbols/modules \
and the rest of the unaccounted data.\
"
      in
      [ iframe; H.br (); make_description descr_txt ]
    )
    in
    let a_paragraph = H.[ a_style "text-align: center" ] in
    List.flatten [
      [ H.p ~a:a_paragraph viz_deps];
      if not @@ contains_debug_bin artifacts then [] else [
        H.p ~a:a_paragraph @@ Lazy.force viz_treemap ];
    ]

  let make
      ~job_name
      ~(build:Builder_db.Build.t)
      ~artifacts
      ~main_binary
      ~solo5_manifest
      ~same_input_same_output
      ~different_input_same_output
      ~same_input_different_output
      ~latest ~next ~previous
    =
    let delta = Ptime.diff build.finish build.start in
    let right_column = make_viz_section ~job_name ~artifacts ~uuid:build.uuid in
    let left_column =
      make_build_info
        ~job_name
        ~delta
        ~build
        ~artifacts
        ~main_binary
        ~solo5_manifest
        ~same_input_same_output
        ~different_input_same_output
        ~same_input_different_output
        ~latest ~next ~previous
    in
    let style_grid = H.a_style "display: flex; " in
    let style_col_left =
      H.a_style "width: 45em; min-width: 43em;" in
    let style_col_right = H.a_style "width: 50%" in
    let body = [
        H.h1 [txtf "Job %s" job_name];
        H.div~a:[ style_grid ] [
          H.div~a:[ style_col_left ]  left_column;
          H.div~a:[ style_col_right ] right_column
        ]
    ]
    in
    layout
      ~nav:(`Build (job_name, build))
      ~title:(Fmt.str "Job %s %a" job_name pp_ptime build.start)
      ~manual_width:true
      body

end

let key_values xs =
  List.concat_map (fun (k, v) -> [ txtf "%s %s" k v ; H.br () ]) xs

let key_value_changes xs =
  List.concat_map (fun (k, v, v') -> [ txtf "%s %s->%s" k v v' ; H.br () ]) xs

let packages packages =
  OpamPackage.Set.elements packages
  |> List.concat_map (fun p -> [
        txtf "%a" Opamdiff.pp_opampackage p;
        H.br ();
      ])

let package_diffs diffs =
  List.concat_map (fun pd -> [
        txtf "%a" Opamdiff.pp_version_diff pd;
        H.br ();
      ])
    diffs

let opam_diffs diffs =
  List.concat_map (fun pd ->
      H.h4 [ txtf "%a" Opamdiff.pp_opam_diff pd ] ::
      (match pd.Opamdiff.build with None -> [] | Some a ->
          let l, r = Opamdiff.commands_to_strings a in
          [
            H.h5 [ H.txt "build instruction (without common prefix) \
                          modifications, old:" ] ;
            H.code (List.concat_map (fun s -> [ H.txt s ; H.br () ]) l) ;
            H.h5 [ H.txt "new" ] ;
            H.code (List.concat_map (fun s -> [ H.txt s ; H.br () ]) r)
          ]) @
      (match pd.Opamdiff.install with None -> [] | Some a ->
          let l, r = Opamdiff.commands_to_strings a in
          [
            H.h5 [ H.txt "install instruction (without common prefix) \
                          modifications, old:" ] ;
            H.code (List.concat_map (fun s -> [ H.txt s ; H.br () ]) l) ;
            H.h5 [ H.txt "new" ] ;
            H.code (List.concat_map (fun s -> [ H.txt s ; H.br () ]) r)
          ]) @
      (match pd.Opamdiff.url with None -> [] | Some a ->
          let l, r = Opamdiff.opt_url_to_string a in
          [
            H.h5 [ H.txt "URL" ] ;
            txtf "old: %s" l;
            H.br ();
            txtf "new: %s" r
          ]) @
      [ H.br () ])
    diffs

let compare_builds
    ~job_left
    ~job_right
    ~(build_left : Builder_db.Build.t)
    ~(build_right : Builder_db.Build.t)
    ~env_diff:(added_env, removed_env, changed_env)
    ~pkg_diff:(added_pkgs, removed_pkgs, changed_pkgs)
    ~opam_diff:(same, opam_diff, version_diff, left, right)
  =
  layout
    ~nav:(`Comparison ((job_left, build_left), (job_right, build_right)))
    ~title:(Fmt.str "Comparing builds %a and %a"
              Uuidm.pp build_left.uuid Uuidm.pp build_right.uuid)
    ([
      H.h1 [H.txt "Comparing builds"];
      H.h2 [
        H.txt "Builds ";
        H.a ~a:H.[ a_href @@
                   Link.Job_build.make
                     ~job_name:job_left
                     ~build:build_left.uuid () ]
          [ txtf "%s@%a %a"
              job_left
              pp_ptime build_left.start
              pp_platform (Some build_left.platform)];
        H.txt " and ";
        H.a ~a:H.[ a_href @@
                   Link.Job_build.make
                     ~job_name:job_right
                     ~build:build_right.uuid () ]
          [ txtf "%s@%a %a"
              job_right
              pp_ptime build_right.start
              pp_platform (Some build_right.platform)];
      ];
      H.h3 [ H.a ~a:H.[
          a_href @@ Link.Compare_builds.make
            ~left:build_right.uuid
            ~right:build_left.uuid () ]
          [H.txt "Compare in reverse direction"]] ;
      H.ul [
        H.li [
          H.a ~a:H.[a_href "#opam-packages-removed"]
            [txtf "%d opam packages removed"
               (OpamPackage.Set.cardinal left)]
        ];
        H.li [
          H.a ~a:H.[a_href "#opam-packages-installed"]
            [txtf "%d new opam packages installed"
               (OpamPackage.Set.cardinal right)]
        ];
        H.li [
          H.a ~a:H.[a_href "#opam-packages-version-diff"]
            [txtf "%d opam packages with version changes"
               (List.length version_diff)]
        ];
        H.li [
          H.a ~a:H.[a_href "#opam-packages-opam-diff"]
            [txtf "%d opam packages with changes in their opam file"
               (List.length opam_diff)]
        ];
        H.li [
          H.a ~a:H.[a_href "#opam-packages-unchanged"]
            [txtf "%d opam packages unchanged" (OpamPackage.Set.cardinal same)]
        ];
        H.li [
          H.a ~a:H.[a_href "#env-added"]
            [ txtf "%d environment variables added" (List.length added_env)]
        ];
        H.li [
          H.a ~a:H.[a_href "#env-removed"]
            [ txtf "%d environment variables removed" (List.length removed_env)]
        ];
        H.li [
          H.a ~a:H.[a_href "#env-changed"]
            [ txtf "%d environment variables changed" (List.length changed_env)]
        ];
        H.li [
          H.a ~a:H.[a_href "#pkgs-added"]
            [ txtf "%d system packages added" (List.length added_pkgs)]
        ];
        H.li [
          H.a ~a:H.[a_href "#pkgs-removed"]
            [ txtf "%d system packages removed" (List.length removed_pkgs)]
        ];
        H.li [
          H.a ~a:H.[a_href "#pkgs-changed"]
            [ txtf "%d system packages changed" (List.length changed_pkgs)]
        ];
      ];
      H.h3 ~a:H.[a_id "opam-packages-removed"]
        [H.txt "Opam packages removed"];
      H.code (packages left);
      H.h3 ~a:H.[a_id "opam-packages-installed"]
        [H.txt "New opam packages installed"];
      H.code (packages right);
      H.h3 ~a:H.[a_id "opam-packages-version-diff"]
        [H.txt "Opam packages with version changes"];
      H.code (package_diffs version_diff);
      H.h3 ~a:H.[a_id "opam-packages-opam-diff"]
        [H.txt "Opam packages with changes in their opam file"]] @
      opam_diffs opam_diff @ [
        H.h3 ~a:H.[a_id "opam-packages-unchanged"]
          [H.txt "Unchanged opam packages"];
        H.code (packages same);
        H.h3 ~a:H.[a_id "env-added"] [H.txt "Environment variables added"];
        H.code (key_values added_env);
        H.h3 ~a:H.[a_id "env-removed"] [H.txt "Environment variables removed"];
        H.code (key_values removed_env);
        H.h3 ~a:H.[a_id "env-changed"] [H.txt "Environment variables changed"];
        H.code (key_value_changes changed_env);
        H.h3 ~a:H.[a_id "pkgs-added"] [H.txt "System packages added"];
        H.code (key_values added_pkgs);
        H.h3 ~a:H.[a_id "pkgs-removed"] [H.txt "System packages removed"];
        H.code (key_values removed_pkgs);
        H.h3 ~a:H.[a_id "pkgs-changed"] [H.txt "System packages changed"];
        H.code (key_value_changes changed_pkgs);
      ])

let failed_builds ~start ~count builds =
  let build (job_name, build) =
    H.li [
      check_icon build.Builder_db.Build.result;
      txtf " %s %a " job_name pp_platform (Some build.platform);
      H.a ~a:H.[ a_href @@ Link.Job_build.make ~job_name ~build:build.uuid () ]
        [txtf "%a" pp_ptime build.start];
      txtf " %a" Builder.pp_execution_result build.result;
    ]
  in
  layout ~title:"Failed builds"
    ([
      H.h1 [H.txt "Failed builds"];
      H.ul (List.map build builds);
      H.p [ txtf "View the next %d failed builds " count;
            H.a ~a:H.[
                a_href @@ Link.Failed_builds.make
                  ~count ~start:(start + count) () ]
              [ H.txt "here"];
            H.txt ".";
          ]
    ])


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
        a_class ["text-primary-500 cursor-pointer"];
        a_titlef "%a" Builder.pp_execution_result result;
      ]
      [H.txt "☑"]
  | _ ->
    H.span ~a:H.[
        a_class ["text-secondary-500 cursor-pointer"];
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
  let static_css = Styles.static_css :: Option.to_list include_static_css
  in
  let body =
    let _style_grid_container = H.a_style "\
      display: flex;
      align-items: center;
      justify-content: center;
      min-width: 83em;
    "
    and _style_grid = H.a_style @@
      if manual_width then "" else "\
        width: 76%;\
      "
    in
    [ H.div ~a:[  ]
        [ H.div ~a:[  ] body ]]
  in
  H.html
    (H.head (H.title (H.txt title))
       [H.style ~a:H.[a_mime_type "text/css"] static_css])
    (H.body ~a:[H.a_class ["bg-gray-50 dark:bg-black-molly  text-gray-800 dark:text-gray-50 mx-auto p-10"]] [
        H.div ~a:[H.a_class ["fixed text-center"]; H.a_style ""] [
          H.img ~a:[H.a_class [""]; H.a_id "robur-logo"] ~src:"https://i.ibb.co/Y4YsvcDb/robur-logo.png" ~alt:"Robur Logo" ()
        ];
        H.div ~a:[H.a_class ["max-w-7xl mx-auto"]] [
          H.(div ~a:[a_class ["flex justify-between items-center"]] [
            div [breadcrumb];
            div ~a:[a_class ["flex items-center space-x-4"]] [
              form ~a:[a_action "/hash"; a_method `Get; a_class ["mt-6 p-4"]] [
                label ~a:[a_class ["block text-lg font-semibold mb-2 text-right"]] [
                      txt "Search artifact by SHA256";
                    ];
                div ~a:[a_class ["w-full flex space-x-2 justify-end justify-items-center items-center"]] [
                  div  [
                    input ~a:[
                      a_input_type `Search;
                      a_id "sha256";
                      a_required ();
                      a_name "sha256";
                      a_class ["w-full border bg-gray-200 text-gray-800 rounded px-3 py-2 focus:ring-0 focus:ring-primary-500"]
                    ] ()
                  ];
                  div ~a:[a_class ["text-center"]] [
                    input ~a:[
                      a_input_type `Submit;
                      a_value "Search";
                      a_class ["mt-3 bg-primary-500 text-gray-50 cursor-pointer font-bold py-2 px-4 rounded hover:bg-primary-800"]
                    ] ()
                  ]
                ]
              ];
              button ~a:[
                a_id "theme-toggle";
                a_class ["p-2 rounded-full border border-gray-300 bg-gray-100 dark:bg-gray-700 dark:text-white hover:bg-gray-200 dark:hover:bg-gray-600"];
              ] [
                txt "🌙"
              ]
            ]
          ]);
          H.main body
        ];

        H.script
        (H.txt "
          document.addEventListener('DOMContentLoaded', function () {
            const themeToggle = document.getElementById('theme-toggle');
            const html = document.documentElement;
            const logo = document.getElementById('robur-logo'); // Ensure the image has this ID

            function updateTheme() {
              const isDark = html.classList.contains('dark');
              themeToggle.innerText = isDark ? '☀️' : '🌑';
              logo.src = isDark
                ? 'https://i.ibb.co/Y4YsvcDb/robur-logo.png'  // Dark mode logo
                : 'https://i.ibb.co/r2DRDdTt/robur-logo-black-writing.png'; // Light mode logo
            }

            function toggleTheme() {
              html.classList.toggle('dark');
              localStorage.setItem('theme', html.classList.contains('dark') ? 'dark' : 'light');
              updateTheme();
            }

            // Load user preference from localStorage
            if (localStorage.getItem('theme') === 'dark') {
              html.classList.add('dark');
            }

            // Set correct icon and logo on load
            updateTheme();

            // Attach event listener (in case onclick fails)
            themeToggle.addEventListener('click', toggleTheme);
          });
      ")

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
    ~file:{ Builder_db.filepath; sha256; size }
  =
  let artifact_link =
    Link.Job_build_artifact.make
      ~job_name
      ~build:build.Builder_db.Build.uuid
      ~artifact:(`File filepath) ()
  in
  H.(div [
    a ~a:[a_href artifact_link; a_class ["text-primary-500 underline"]] [
      (if basename then txt ("Download " ^ Fpath.basename filepath)
      else txtf "Download %a" Fpath.pp filepath);
      txtf " (%a)" Fmt.byte_size size;
    ];
    br ();
    code [txtf "SHA256:%s" (Ohex.encode sha256)];
  ])

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
  let static_css = Styles.static_css :: [ Tyxml.Html.Unsafe.data "\
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

  let make_header =
    H.([
      div ~a:[a_class ["header container mx-auto px-4 py-8 text-gray-800"]] [
        (* Logo Section *)
        div ~a:[a_class ["flex items-center mb-6"]] [
          ];
          h1 ~a:[a_class ["text-7xl font-bold text-primary-600 text-center"]]
            [txt "Reproducible OPAM Builds"]
        ];

        div ~a:[a_class ["grid grid-cols-2 gap-3"]] [
          div [
            p ~a:[a_class ["text-lg mb-4"]] [
              txt "This website offers binary MirageOS unikernels and supplementary OS packages. ";
              txt "If you want to use our binary packages and setup unikernels, follow ";
              a ~a:[a_href "https://robur.coop/Projects/Reproducible_builds"; a_class ["text-primary-500 underline font-mono"]]
                [txt "these instructions"];
              txt "."
            ];
            p ~a:[a_class ["text-lg mb-4"]] [
              txt "The unikernels are statically linked executables where the execution target is ";
              txt "independent of the build platform - so even if they're compiled on a FreeBSD ";
              txt "system they can be run on a Linux or OpenBSD host. Many are executed using a ";
              a ~a:[a_href "https://github.com/solo5/solo5"; a_class ["text-primary-500 underline font-mono"]]
                [txt "solo5"];
              txt " tender."
            ];
          ];
          div [
            p ~a:[a_class ["text-lg mt-6 mb-4"]] [
              txt "A persistent link to the latest successful build is available as ";
              code ~a:[a_class ["px-2 py-1 rounded text-sm font-mono text-primary-500"]]
                [txt "/job/*jobname*/build/latest/"];
              txt ". Each build can be reproduced with ";
              a ~a:[a_href "https://github.com/robur-coop/orb/"; a_class ["text-primary-500 underline font-mono"]]
                [txt "orb"];
              txt "."
            ];
            p ~a:[a_class ["text-lg mb-4"]] [
              txt "The builds are scheduled and executed daily by ";
              a ~a:[a_href "https://github.com/robur-coop/builder/"; a_class ["text-primary-500 underline font-mono"]]
                [txt "builder"];
              txt ". This web interface is ";
              a ~a:[a_href "https://git.robur.coop/robur/builder-web/"; a_class ["text-primary-500 underline font-mono"]]
                [txt "builder-web"];
              txt ". Read further information ";
              a ~a:[a_href "https://robur.coop/Projects/Reproducible_builds"; a_class ["text-primary-500 underline font-mono"]]
                [txt "on our project page"];
              txt "."
            ];
            p ~a:[a_class ["text-lg mt-6 mb-4"]] [
              txt "This work has been funded by the European Union under the ";
              a ~a:[a_href "https://pointer.ngi.eu"; a_class ["text-primary-500 underline font-mono"]]
                [txt "NGI Pointer"];
              txt " program. Contact team AT robur.coop if you have questions or suggestions."
            ];
          ];
        ];
        div ~a:[a_class ["my-4"]] [
            h2 ~a:[a_class ["text-xl font-semibold mt-6 mb-2"]]
            [txt "Execution Environments"];
            ul ~a:[a_class ["list-disc list-inside text-lg space-y-2 font-mono"]] [
              li [span ~a:[a_class ["text-primary-500"]] [txt ".spt: "]; txt "sandboxed process - requires solo5-spt (Linux with seccomp)"];
              li [span ~a:[a_class ["text-primary-500"]] [txt ".xen: "]; txt "Xen PVH virtual machine (on a Xen or QubesOS host)"];
              li [span ~a:[a_class ["text-primary-500"]] [txt ".virtio: "]; txt "any virtio environment (qemu, GCE, KVM, BHyve)"];
              li [span ~a:[a_class ["text-primary-500"]] [txt ".muem: "]; txt "on muen"]
            ];
        ]
      ]
    )

    let make_platform_builds ~job_name (platform, latest_build, latest_artifact) =
      H.([
          div ~a:[a_class ["grid grid-cols-3 space-y-2 p-2 rounded-lg"]]
            [
              div ~a:[a_class ["flex items-center space-x-2"]]
                [
                  check_icon latest_build.Builder_db.Build.result;
                  a ~a:[
                    a_href @@ Link.Job.make ~job_name ~queries:[ `Platform platform ] ();
                    a_class ["text-primary-500 underline font-medium"]
                  ]
                  [txt platform]
                ];
              div ~a:[a_class ["text-gray-300"]]
                [ a ~a:[
                    a_href @@ Link.Job_build.make
                      ~job_name
                      ~build:latest_build.Builder_db.Build.uuid ();
                    a_class ["underline text-primary-500"]
                  ]
                  [txtf "%a" pp_ptime latest_build.Builder_db.Build.start]; ];

              div ~a:[a_class [""]]
                [ artifact ~basename:true ~job_name ~build:latest_build ~file:latest_artifact ];
            ];
        ])

      let make_jobs jobs =
        H.(table
              ~a:[a_class ["table-auto min-w-full"]]
              ~thead:
                (thead
                    [
                      tr
                        [];
                    ])
        (List.map (fun (job_name, synopsis, platform_builds) ->
              tr ~a:[a_class ["divide-y divide-gray-600"]]
                [
                  td
                    ~a:[a_class ["px-6 py-4 font-medium"]]
                    [
                      a ~a:[a_href ("/job/" ^ job_name ^ "/"); a_class ["text-primary-500 font-bold"]]
                        [txt job_name];
                      br();
                      txt (Option.value ~default:"" synopsis);
                    ];

                  td
                    ~a:[a_class ["px-6 py-4 text-gray-400"]]
                    [
                      div ~a:[a_class ["flex flex-col text-wrap"]]
                        (List.concat_map (make_platform_builds ~job_name) platform_builds);
                    ];
                ];
                )
            jobs))

  let make_body section_job_map =
    let aux  section jobs acc =
      acc @ [
      H.div ~a:[H.a_class ["my-4 py-4 divide-y divide-gray-300"]] [
          H.h2 ~a:[H.a_class ["text-xl uppercase font-bold my-4"]] [ H.txt section ];
          make_jobs jobs;
        ]
      ]
    in
    Utils.String_map.fold aux section_job_map []

  let make_failed_builds =
    [ H.div ~a:H.[a_class ["flex justify-center mt-6"]] [
        H.a ~a:H.[a_href "/failed-builds";
                  a_class ["text-secondary-500 underline font-semibold"]]
          [H.txt "View Latest Failed Builds"];
      ]]

  let make_all_or_active all =
    [ H.div ~a:H.[a_class ["flex justify-center mt-6"]] [
        H.a ~a:H.[a_href (if all then "/" else "/all-builds");
                  a_class ["text-primary-500 underline font-semibold"]]
          [H.txt (if all then "View Active Jobs" else "View All Jobs")];
      ]]

  let make ~all section_job_map =
    layout ~title:"Reproducible OPAM builds"
      (make_header
        @ make_body section_job_map
        @ make_failed_builds
        @ make_all_or_active all)


  let make_json ~all:_ section_job_map =
    let all_jobs =
      Utils.String_map.fold
        (fun _section jobs acc ->
           List.map (fun (job_name, _, _) -> `String job_name) jobs @ acc)
        section_job_map []
    in
    let by_section =
      Utils.String_map.fold
        (fun section jobs acc ->
           (section, `List (List.map (fun (job_name, _, _) -> `String job_name) jobs)) :: acc)
        section_job_map []
    in
    `Assoc [
      "jobs", `List all_jobs;
      "jobs_by_section", `Assoc by_section;
    ]
end

module Job = struct

  let make_header ~job_name ~platform ~readme =
    H.(h1 ~a:[a_class ["text-4xl font-bold text-center my-4"]] [txtf "Job %s %a" job_name pp_platform platform])
    :: (
      match readme with
      | None -> []
      | Some data ->
        [
          H.(div ~a:[a_class ["flex justify-between items-center"]] [
            h2 ~a:[a_id "readme"; a_class ["text-2xl"]] [txt "README"];
            a ~a:[a_href "#builds"; a_class ["text-primary-500 underline"]] [txt "Skip to builds"];
           ]);
          H.Unsafe.data (Utils.md_to_html ~adjust_heading:2 data)
        ]
    )

  let make_build ~job_name (build, main_binary) =
    H.(li ~a:[a_class ["my-4 p-4 border-t-1"]] (
      [
        div ~a:[a_class ["flex my-2"]] [
          check_icon build.Builder_db.Build.result;
          p ~a:[a_class ["text-xl px-2"]] [txtf " %s " build.platform;];
        ];
        a ~a:[
            a_href @@ Link.Job_build.make
              ~job_name
              ~build:build.Builder_db.Build.uuid (); a_class ["text-primary-500 underline font-mono my-2"] ]
          [
            txtf "%a" pp_ptime build.Builder_db.Build.start;
          ];
      ]
      @ match main_binary with
      | Some main_binary ->
        [artifact
          ~basename:true
          ~job_name
          ~build
          ~file:main_binary]
      | None ->
        [ txtf "Build failure: %a" Builder.pp_execution_result
            build.Builder_db.Build.result ]
    ))

  let make_builds ~failed ~job_name ~platform builds =
    [
     H.(div ~a:[a_class ["flex justify-between items-center"]] [
      h2 ~a:[a_id "builds"; a_class ["text-2xl"]] [txt "Builds"];
      a ~a:[a_href "#readme"; a_class ["text-primary-500 underline"]] [txt "Back to readme"];
     ]);
      H.ul (builds |> List.map (make_build ~job_name));
      let queries =
        platform |> Option.map (fun p -> `Platform p) |> Option.to_list
      in
      if failed then
        H.p [
          H.txt "Excluding failed builds " ;
          H.a ~a:H.[
              a_href @@ Link.Job.make ~job_name ~queries (); a_class ["text-primary-500 underline font-mono"]
            ]
            [H.txt "here"] ;
          H.txt "." ]
      else
        H.p [
          H.txt "Including failed builds " ;
          H.a ~a:H.[
              a_href @@ Link.Job.make_failed ~job_name ~queries (); a_class ["text-secondary-500 underline font-mono"]
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

  let make_json ~failed:_ ~job_name:_ ~platform:_ ~readme:_ builds =
    (* For now we will ignore most arguments. It's to keep the arguments the
       same as [make]. This is subject to change. *)
    let build (build, main_binary) =
      let main_binary =
        match main_binary with
        | None -> `Null
        | Some { Builder_db.filepath; sha256; size } ->
          `Assoc [
            "filename", `String (Fpath.basename filepath);
            "sha256", `String (Ohex.encode sha256);
            "size", `Int size;
          ]
      in
      `Assoc [
        "uuid", `String (Uuidm.to_string build.Builder_db.Build.uuid);
        "main_binary", main_binary
      ]
    in
    `Assoc [
      "builds", `List (List.map build builds);
    ]
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
      let sha256_hex = Ohex.encode file.sha256 in
      [
      H.(dt [
          a ~a:[a_href @@ Link.Job_build_artifact.make
                      ~job_name
                      ~build:build_uuid
                      ~artifact:(`File file.filepath) ();
                    a_class ["text-primary-500 underline"]
                   ]
            [code [txtf "%a" Fpath.pp file.filepath; txtf " (%a)" Fmt.byte_size file.size]] ]);
        H.dd ([
            H.code [H.txt "SHA256:"; H.txt sha256_hex];
          ] @
            match main_binary, solo5_manifest with
            | Some main_binary, Some solo5_manifest when main_binary = file ->
              (H.br () :: solo5_devices solo5_manifest)
            | _ -> []);
      ]
    in
    [
      H.(h3 ~a:[a_class ["text-xl font-semibold my-4"]] [txt "Build artifacts"]);
      H.(dl ~a:[a_class ["p-4 my-4"]] (List.concat_map aux artifacts))
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
      H.(div ~a:[a_class ["my-4"]] [
        h3 ~a:[a_class ["text-xl font-semibold"]] [
        txtf "Reproduced by %d builds"
          (List.length (same_input_same_output @ different_input_same_output))];
      ul @@ (
        same_input_same_output_html
        @ different_input_same_output_html
      )
      ])
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
      H.(h3 ~a:[a_class ["my-4 text-xl font-semibold"]] [txt "Comparisons with other builds on the same platform"]);
      let opt_build (ctx, build') =
        match build' with
        | Some b when not (Uuidm.equal build.uuid b.Builder_db.Build.uuid) ->
          [ H.li [ H.txt ctx;
                   H.a ~a:[
                     H.a_href @@ Link.Compare_builds.make
                       ~left:b.uuid
                       ~right:build.uuid () ; H.a_class ["underline text-primary-500 font-mono"] ]
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
      H.(h2 ~a:[a_id "build"; a_class ["text-2xl my-4 font-semibold"]] [txtf "Build %a" pp_ptime build.start]);
      H.(div ~a:[a_class []] [
        table
          ~a:[a_class ["table-auto min-w-full border my-4"]]
          ~thead:
            (thead
                [
                  tr ~a:[a_class ["border"]]
                    [
                      th
                        ~a:
                          [
                            a_class
                              [
                                "px-6 py-2 text-center \
                                font-bold \
                                text-primary-600 uppercase";
                              ];
                          ]
                        [ txt "Platform" ];
                      th
                        ~a:
                          [
                            a_class
                              [
                                "px-6 py-2 text-center \
                                font-bold \
                                text-primary-600 uppercase";
                              ];
                          ]
                        [ txt "Duration" ];
                      th
                        ~a:
                          [
                            a_class
                              [
                                "px-6 py-2 text-center \
                                font-bold \
                                text-primary-600 uppercase";
                              ];
                          ]
                        [ txt "Execution Result" ];
                    ];
                ])
                [
                  tr ~a:[a_class ["text-center"]]
                  [
                    td
                      ~a:
                        [
                          a_class
                            [
                              "px-6 py-1 \
                              font-medium";
                            ];
                        ]
                      [ txtf "%s" build.platform ];
                    td
                      ~a:
                        [
                          a_class
                            [
                              "px-6 py-1 \
                              font-medium";
                            ];
                        ]
                      [ txtf "%a." Ptime.Span.pp delta ];
                    td
                      ~a:
                        [
                          a_class
                            [
                              "px-6 py-1 \
                              font-medium";
                            ];
                        ]
                      [ txtf "%a" Builder.pp_execution_result build.result ];
                  ]
                ];
      ]);
      H.(h3 ~a:[a_class ["text-xl font-semibold my-2"]] [txt "Build info"]);
      H.(div ~a:[a_class ["my-4 flex justify-between items-center"]] [
        div [
          a ~a:[
            a_href @@ Link.Job_build_artifact.make
              ~job_name
              ~build:build.uuid
              ~artifact:`Console ();
              a_class ["text-primary-500 font-mono"]
          ] [H.txt "Console output -->"];
        ];
        div [
          a ~a:[
            a_href @@ Link.Job_build_artifact.make
              ~job_name
              ~build:build.uuid
              ~artifact:`Script ();
              a_class ["text-primary-500 font-mono"]
          ] [txt "Build script -->"];
        ]
      ]);
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
    let _style_grid = H.a_style "display: flex; " in
    let _style_col_left =
      H.a_style "width: 45em; min-width: 43em;" in
    let _style_col_right = H.a_style "width: 50%" in
    let body = [
        H.(h1 ~a:[a_class ["text-4xl font-bold text-center my-4"]] [txtf "Job %s" job_name]);
        H.div~a:[ H.a_class ["grid grid-cols-2 gap-10"] ] [
          H.div~a:[ ]  left_column;
          H.div~a:[  ] right_column
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

let duniverse_dirs dirs =
  List.concat_map (fun p -> [
        txtf "%a" Opamdiff.pp_duniverse_dir p;
        H.br ();
      ]) dirs

let duniverse_diffs diffs =
  List.concat_map (fun p -> [
        txtf "%a" Opamdiff.pp_duniverse_diff p;
        H.br ();
      ]) diffs

let opam_diffs diffs =
   List.concat_map (fun pd ->
      H.h4 ~a:[H.a_class ["text-md font-semibold text-primary-500"]] [ txtf "%a" Opamdiff.pp_opam_diff pd ] ::
      H.pre [ H.code [H.txt pd.diff] ] ::
      H.br () :: [])
    diffs

let compare_builds
    ~job_left
    ~job_right
    ~(build_left : Builder_db.Build.t)
    ~(build_right : Builder_db.Build.t)
    ~env_diff:(added_env, removed_env, changed_env)
    ~pkg_diff:(added_pkgs, removed_pkgs, changed_pkgs)
    ~opam_diff:(opam_diff, version_diff, left, right, duniverse)
  =
  let items, data =
    List.fold_left (fun (items, data) (id, txt, amount, code) ->
        let id_href = "#" ^ id in
        if amount = 0 then
          items, data
        else
          H.li [ H.a ~a:[H.a_href id_href; H.a_class ["underline text-primary-500 font-mono"]] [txtf "%d %s" amount txt] ] :: items,
          data @ H.h3 ~a:[H.a_id id; H.a_class ["text-xl font-semibold my-4"]] [H.txt txt] :: code)
      ([], [])
      ([ ("opam-packages-removed", "Opam packages removed",
          OpamPackage.Set.cardinal left, [ H.(code ~a:[a_class ["code-diff"]] (packages left)) ]) ;
         ("opam-packages-installede", "New opam packages installed",
          OpamPackage.Set.cardinal right, [ H.(code ~a:[a_class ["code-diff"]] (packages right)) ]) ;
         ("opam-packages-version-diff", "Opam packages with version changes",
          List.length version_diff, [ H.(code ~a:[a_class ["code-diff"]] (package_diffs version_diff)) ]) ;
       ] @ (match duniverse with
          | Ok (duniverse_left, duniverse_right, duniverse_content_diff) ->
            [
              ("duniverse-dirs-removed", "Duniverse directories removed",
               List.length duniverse_left, [ H.(code ~a:[a_class ["code-diff"]] (duniverse_dirs duniverse_left)) ]) ;
              ("duniverse-dirs-installed", "New duniverse directories installed",
               List.length duniverse_right, [ H.(code ~a:[a_class ["code-diff"]] (duniverse_dirs duniverse_right)) ]) ;
              ("duniverse-dirs-content-diff", "Duniverse directories with content changes",
               List.length duniverse_content_diff, [ H.(code ~a:[a_class ["code-diff"]] (duniverse_diffs duniverse_content_diff)) ]) ;
            ]
          | Error `Msg msg -> [ "duniverse-dirs-error", "Duniverse parsing error", 1,  [ H.txt msg ] ]
        ) @ [
         ("opam-packages-opam-diff", "Opam packages with changes in their opam file",
          List.length opam_diff, opam_diffs opam_diff) ;
         ("env-removed", "Environment variables removed",
          List.length removed_env, [ H.(code ~a:[a_class ["code-diff"]] (key_values removed_env)) ]) ;
         ("env-added", "New environment variables added",
          List.length added_env, [ H.(code ~a:[a_class ["code-diff"]] (key_values added_env)) ]) ;
         ("env-changed", "Environment variables changed",
          List.length changed_env, [ H.(code ~a:[a_class ["code-diff"]] (key_value_changes changed_env)) ]) ;
         ("pkgs-removed", "System packages removed",
          List.length removed_pkgs, [ H.(code ~a:[a_class ["code-diff"]] (key_values removed_pkgs)) ]) ;
         ("pkgs-added", "New system packages added",
          List.length added_pkgs, [ H.(code ~a:[a_class ["code-diff"]] (key_values added_pkgs)) ]) ;
         ("pkgs-changed", "System packages changed",
          List.length changed_pkgs, [ H.(code ~a:[a_class ["code-diff"]] (key_value_changes changed_pkgs)) ]) ;
       ])
  in
  layout
    ~nav:(`Comparison ((job_left, build_left), (job_right, build_right)))
    ~title:(Fmt.str "Comparing builds %a and %a"
              Uuidm.pp build_left.uuid Uuidm.pp build_right.uuid)
    ([
      H.(h1 ~a:[a_class ["text-center text-2xl font-semibold my-4"]] [txt "Comparing builds"]);
      H.(h2 ~a:[a_class ["text-center text-xl font-semibold my-4"]] [
        txt "Builds ";
        (a ~a:[ a_href @@
                   Link.Job_build.make
                     ~job_name:job_left
                     ~build:build_left.uuid (); a_class ["text-primary-500 underline font-mono"] ]
          [ txtf "%s@%a %a"
              job_left
              pp_ptime build_left.start
              pp_platform (Some build_left.platform)]);
        txt " and ";
        a ~a:[ a_href @@
                   Link.Job_build.make
                     ~job_name:job_right
                     ~build:build_right.uuid (); a_class ["text-primary-500 underline font-mono"] ]
          [ txtf "%s@%a %a"
              job_right
              pp_ptime build_right.start
              pp_platform (Some build_right.platform)];
      ]);
      H.(h3 ~a:[a_class ["text-right"]] [ a ~a:[
          a_href @@ Link.Compare_builds.make
            ~left:build_right.uuid
            ~right:build_left.uuid (); a_class ["text-primary-500 underline font-mono"]  ]
          [txt "Compare in reverse direction"]]) ;
      H.ul (List.rev items) ] @ data)

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
    (match builds with
     | [] ->
       [
         H.h1 [H.txt "No failed builds to list"];
         H.p [H.txt "🥳"];
       ]
     | _ :: _ ->
       [
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

let robots_txt =
{robots_txt|User-agent: *
Disallow: /job/*/build/*/f/
Disallow: /job/*/build/*/main-binary
Disallow: /job/*/build/*/script
Disallow: /job/*/build/*/console
Disallow: /job/*/build/*/all.tar.gz
Disallow: /job/*/build/*/exec
Disallow: /compare/
|robots_txt}

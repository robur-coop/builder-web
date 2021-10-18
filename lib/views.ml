open Tyxml.Html

let pp_ptime = Ptime.pp_human ()

let txtf fmt = Fmt.kstr txt fmt
let a_titlef fmt = Fmt.kstr a_title fmt

let check_icon result =
  match result with
  | Builder.Exited 0 ->
    span ~a:[
      a_style "color: green; cursor: pointer;";
      a_titlef "%a" Builder.pp_execution_result result;
    ]
      [txt "☑"]
  | _ ->
    span ~a:[
      a_style "color: red; cursor: pointer;";
      a_titlef "%a" Builder.pp_execution_result result;
    ]
      [txt "☒"]

let layout ~title:title_ body_ =
  html
    (head (title (txt title_))
       [style ~a:[a_mime_type "text/css"]
          [
            txt "body {\
                 margin: 40px auto;\
                 line-height: 1.6;\
                 color: #444;\
                 padding: 0 10px;\
                 }";
            txt "h1,h2,h3{line-height:1.2}";
            txt ".output-ts {\
                 white-space: nowrap;\
                 cursor: pointer;\
                 user-select: none;\
                 }";
            txt ".output-ts a {text-decoration: none;}";
            txt ".output-ts a:hover {text-decoration: underline;}";
            txt ".output-code {\
                 overflow: visible;\
                 white-space: pre;\
                 }";
            txt ".toggleable {\
                 display: none;\
                 }";
            txt ".toggleable-descr {\
                 cursor: pointer;\
                 text-decoration: underline;\
                 user-select: none;\
                 }";
            txt ":checked + .toggleable {\
                 display: block;\
                 }";
          ]])
    (body body_)

let toggleable ?(hidden=true) id description content =
  let checked = if hidden then [] else [a_checked ()] in
  div [
    label
      ~a:[
        a_label_for id;
        a_class ["toggleable-descr"];
      ]
      [txt description];
    input
      ~a:(checked @ [
          a_input_type `Checkbox;
          a_id id;
          a_style "display: none;";
        ]) ();
    div
      ~a:[
        a_class ["toggleable"]
      ]
      content;
  ]

let artifact ?(basename=false) job_name build { Builder_db.filepath; localpath = _; sha256; size } =
  [
    a ~a:[a_href (Fmt.str "/job/%s/build/%a/f/%a"
                    job_name
                    Uuidm.pp build.Builder_db.Build.uuid
                    Fpath.pp filepath)]
      [if basename
       then txt (Fpath.basename filepath)
       else txtf "%a" Fpath.pp filepath];
    txt " ";
    code [txtf "SHA256:%a" Hex.pp (Hex.of_cstruct sha256)];
    txtf " (%a)" Fmt.byte_size size;
  ]



let builder section_job_map =
  layout ~title:"Reproducible OPAM builds"
    ([ h1 [txt "Reproducible OPAM builds"];
       p [ txt "This website offers binary MirageOS unikernels and supplementary OS packages." ];
       p [ txt {|Following is a list of jobs that are built daily. A persistent link to the latest successful build is available as /job/*jobname*/build/latest/. All builds can be reproduced with |} ;
           a ~a:[a_href "https://github.com/roburio/orb/"] [txt "orb"];
           txt ". The builds are scheduled and executed by ";
           a ~a:[a_href "https://github.com/roburio/builder/"] [txt "builder"];
           txt ". The web interface is ";
           a ~a:[a_href "https://git.robur.io/robur/builder-web/"] [txt "builder-web"];
           txt ". Contact team@robur.coop if you have any questions or suggestions.";
       ];
       form ~a:[a_action "/hash"; a_method `Get]
         [
           label [
             txt "Search artifact by SHA256";
             br ();
             input ~a:[
               a_input_type `Search;
               a_id "sha256";
               a_name "sha256";
             ] ();
           ];
           input ~a:[
             a_input_type `Submit;
             a_value "Search";
           ] ();
         ];
       ] @
       Utils.String_map.fold (fun section jobs acc ->
         acc @ [
           h2 [ txt section ];
           ul (List.map (fun (job_name, synopsis, latest_build, latest_artifact) ->
               li ([
                   a ~a:[a_href ("job/" ^ job_name ^ "/")]
                     [txt job_name];
                   txt " ";
                   check_icon latest_build.Builder_db.Build.result;
                   br ();
                   txt (Option.value ~default:"" synopsis);
                   br ();
                   a ~a:[a_href (Fmt.str "job/%s/build/%a/" job_name Uuidm.pp
                                   latest_build.Builder_db.Build.uuid)]
                     [txtf "%a" (Ptime.pp_human ()) latest_build.Builder_db.Build.start];
                   txt " ";
                 ] @ match latest_artifact with
                 | Some main_binary ->
                   artifact ~basename:true job_name latest_build main_binary
                 | None ->
                   [
                     txtf "Build failed";
                   ])) jobs)
        ])
        section_job_map
        [])

let job name readme builds =
  layout ~title:(Printf.sprintf "Job %s" name)
    ((h1 [txtf "Job %s" name] ::
      (match readme with
       | None -> []
       | Some data ->
         [
           h2 ~a:[a_id "readme"] [txt "README"];
           a ~a:[a_href "#builds"] [txt "Skip to builds"];
           Unsafe.data Omd.(to_html (of_string data))
         ])) @
     [
      h2 ~a:[a_id "builds"] [txt "Builds"];
      a ~a:[a_href "#readme"] [txt "Back to readme"];
      p [
        txtf "Currently %d builds."
          (List.length builds)
      ];
      ul (List.map (fun (build, main_binary) ->
          li ([
              a ~a:[a_href Fpath.(to_string (v "build" / Uuidm.to_string build.Builder_db.Build.uuid / ""))]
                [
                  txtf "%a" (Ptime.pp_human ()) build.Builder_db.Build.start;
                ];
              txt " ";
              check_icon build.result;
              br ();
            ] @ match main_binary with
            | Some main_binary ->
              artifact ~basename:true name build main_binary
            | None ->
              [
                txtf "Build failed";
              ]))
          builds);

    ])

let job_build
  name
  readme
  { Builder_db.Build.uuid; start; finish; result; _ }
  artifacts
  same_input_same_output different_input_same_output same_input_different_output
  latest_uuid
  =
  let delta = Ptime.diff finish start in
  let successful_build = match result with Builder.Exited 0 -> true | _ -> false in
  layout ~title:(Fmt.str "Job %s %a" name pp_ptime start)
    ((h1 [txtf "Job %s" name] ::
      (match readme with
       | None -> []
       | Some data ->
         [
           h2 ~a:[a_id "readme"] [txt "README"];
           a ~a:[a_href "#build"] [txt "Skip to build"];
           Unsafe.data Omd.(to_html (of_string data))
         ])) @
    [
      h2 ~a:[a_id "build"] [txtf "Build %a" pp_ptime start];
      a ~a:[a_href "#readme"] [txt "Back to readme"];
      p [txtf "Build took %a." Ptime.Span.pp delta ];
      p [txtf "Execution result: %a." Builder.pp_execution_result result]; ] @
      (match same_input_same_output with [] -> [] | xs -> [
         h3 [ txt "Reproduced by builds"] ;
         p (List.concat_map (fun { Builder_db.Build.start ; uuid ; _ } ->
             [ a ~a:[Fmt.kstr a_href "/job/%s/build/%a" name Uuidm.pp uuid]
                [txtf "%a" pp_ptime start] ;
               txt ", " ])
         xs) ] ) @ [
      h3 [txt "Build info"];
      ul [
        li [ a ~a:[Fmt.kstr a_href "/job/%s/build/%a/console" name Uuidm.pp uuid]
               [txt "Console output"];
           ];
        li [ a ~a:[Fmt.kstr a_href "/job/%s/build/%a/script" name Uuidm.pp uuid]
               [txt "Build script"];
           ]
      ];
      h3 [txt "Comparisons with other builds"];
      p
        ((match latest_uuid with
          | Some latest_uuid when successful_build && not (Uuidm.equal latest_uuid uuid) ->
            [ a ~a:[Fmt.kstr a_href "/compare/%a/%a/opam-switch"
                      Uuidm.pp uuid Uuidm.pp latest_uuid]
                [txt "With latest build"] ; br () ]
          | _ -> []) @
         List.concat_map (fun { Builder_db.Build.start = other_start ; uuid = other_uuid ; _ } ->
             let fst, snd = if Ptime.is_later ~than:start other_start then uuid, other_uuid else other_uuid, uuid in
             [ a ~a:[Fmt.kstr a_href "/compare/%a/%a/opam-switch"
                     Uuidm.pp fst Uuidm.pp snd]
                [txtf "With build %a (output is identical binary)" pp_ptime other_start] ; br () ])
           different_input_same_output @
         List.concat_map (fun { Builder_db.Build.start = other_start ; uuid = other_uuid ; _ } ->
             let fst, snd = if Ptime.is_later ~than:start other_start then uuid, other_uuid else other_uuid, uuid in
             [ a ~a:[Fmt.kstr a_href "/compare/%a/%a/opam-switch"
                     Uuidm.pp fst Uuidm.pp snd]
                [txtf "With build %a (same input, different output)" pp_ptime other_start] ; br () ])
           same_input_different_output);
      h3 [txt "Build artifacts"];
      dl (List.concat_map
            (fun { Builder_db.filepath; localpath=_; sha256; size } ->
               let (`Hex sha256_hex) = Hex.of_cstruct sha256 in
               [
                 dt [a
                       ~a:[Fmt.kstr a_href "f/%a" Fpath.pp filepath]
                       [code [txtf "%a" Fpath.pp filepath]]];
                 dd [
                   code [txt "SHA256:"; txt sha256_hex];
                   txtf " (%a)" Fmt.byte_size size;
                 ];
               ])
            artifacts);
      (*
      (* FIXME *)
      h3 [txt "Job script"];
      toggleable "job-script" "Show/hide"
        [ pre [txt script] ];
      h3 [txt "Build log"];
      toggleable ~hidden:false "build-log" "Show/hide build log"
        [
          table
            (List.mapi (fun idx (ts, line) ->
                 let ts_id = "L" ^ string_of_int idx in
                 tr [
                   td ~a:[
                     a_class ["output-ts"];
                     a_id ts_id;
                   ]
                     [a ~a:[a_href ("#"^ts_id); a_class ["output-ts-anchor"]]
                        [code [txtf "%#d ms" (Duration.to_ms (Int64.of_int ts))]]];
                   td ~a:[a_class ["output-code"]]
                     [code [txt line]];
                 ])
                (List.rev console));
        ];
      *)
    ])

let key_values xs =
  List.concat_map (fun (k, v) -> [ txtf "%s %s" k v ; br () ]) xs

let key_value_changes xs =
  List.concat_map (fun (k, v, v') -> [ txtf "%s %s->%s" k v v' ; br () ]) xs

let packages packages =
  OpamPackage.Set.elements packages
  |> List.concat_map (fun p -> [
        txtf "%a" Opamdiff.pp_opampackage p;
        br ();
      ])

let package_diffs diffs =
  List.concat_map (fun pd -> [
        txtf "%a" Opamdiff.pp_version_diff pd;
        br ();
      ])
    diffs

let opam_diffs diffs =
  List.concat_map (fun pd ->
        h4 [ txtf "%a" Opamdiff.pp_opam_diff pd ] ::
        (match pd.Opamdiff.build with None -> [] | Some a ->
          let l, r = Opamdiff.commands_to_strings a in
          [
            h5 [ txt "build instruction (without common prefix) modifications, old:" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) l) ;
            h5 [ txt "new" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) r)
          ]) @
        (match pd.Opamdiff.install with None -> [] | Some a ->
          let l, r = Opamdiff.commands_to_strings a in
          [
            h5 [ txt "install instruction (without common prefix) modifications, old:" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) l) ;
            h5 [ txt "new" ] ;
            code (List.concat_map (fun s -> [ txt s ; br () ]) r)
         ]) @
      (match pd.Opamdiff.url with None -> [] | Some a ->
          let l, r = Opamdiff.opt_url_to_string a in
          [
            h5 [ txt "URL" ] ;
            txtf "old: %s" l;
            br ();
            txtf "new: %s" r
          ]) @
      [ br () ])
    diffs

let compare_opam job_left job_right
    (build_left : Builder_db.Build.t) (build_right : Builder_db.Build.t)
    (added_env, removed_env, changed_env)
    (added_pkgs, removed_pkgs, changed_pkgs)
    (same, opam_diff, version_diff, left, right) =
  layout ~title:(Fmt.str "Comparing opam switches between builds %a and %a"
                   Uuidm.pp build_left.uuid Uuidm.pp build_right.uuid)
    ([
      h1 [txt "Comparing opam switches"];
      h2 [
        txt "Builds ";
        a ~a:[a_href
               (Fmt.str "/job/%s/build/%a/"
                  job_left
                  Uuidm.pp build_left.uuid)]
          [txtf "%a" pp_ptime build_left.start];
        txt " and ";
        a ~a:[a_href
               (Fmt.str "/job/%s/build/%a/"
                  job_right
                  Uuidm.pp build_right.uuid)]
          [txtf "%a" pp_ptime build_right.start];
      ];
      ul [
        li [
          a ~a:[a_href "#packages-removed"]
            [txtf "%d packages removed" (OpamPackage.Set.cardinal left)]
        ];
        li [
          a ~a:[a_href "#packages-installed"]
            [txtf "%d new packages installed" (OpamPackage.Set.cardinal right)]
        ];
        li [
          a ~a:[a_href "#packages-version-diff"]
            [txtf "%d packages with version changes" (List.length version_diff)]
        ];
        li [
          a ~a:[a_href "#packages-opam-diff"]
            [txtf "%d packages with changes in their opam file" (List.length opam_diff)]
        ];
        li [
          a ~a:[a_href "#packages-unchanged"]
            [txtf "%d packages unchanged" (OpamPackage.Set.cardinal same)]
        ];
        li [
         a ~a:[a_href "#env-added"]
            [ txtf "%d environment variables added" (List.length added_env)]
        ];
        li [
         a ~a:[a_href "#env-removed"]
            [ txtf "%d environment variables removed" (List.length removed_env)]
        ];
        li [
         a ~a:[a_href "#env-changed"]
            [ txtf "%d environment variables changed" (List.length changed_env)]
        ];
        li [
         a ~a:[a_href "#pkgs-added"]
            [ txtf "%d system packages added" (List.length added_pkgs)]
        ];
        li [
         a ~a:[a_href "#pkgs-removed"]
            [ txtf "%d system packages removed" (List.length removed_pkgs)]
        ];
        li [
         a ~a:[a_href "#pkgs-changed"]
            [ txtf "%d system packages changed" (List.length changed_pkgs)]
        ];
      ];
      h3 ~a:[a_id "packages-removed"]
        [txt "Packages removed"];
      code (packages left);
      h3 ~a:[a_id "packages-installed"]
        [txt "New packages installed"];
      code (packages right);
      h3 ~a:[a_id "packages-version-diff"]
        [txt "Packages with version changes"];
      code (package_diffs version_diff);
      h3 ~a:[a_id "packages-opam-diff"]
        [txt "Packages with changes in their opam file"]] @
      opam_diffs opam_diff @ [
      h3 ~a:[a_id "packages-unchanged"]
        [txt "Unchanged packages"];
      code (packages same);
      h3 ~a:[a_id "env-added"] [txt "Environment variables added"];
      code (key_values added_env);
      h3 ~a:[a_id "env-removed"] [txt "Environment variables removed"];
      code (key_values removed_env);
      h3 ~a:[a_id "env-changed"] [txt "Environment variables changed"];
      code (key_value_changes changed_env);
      h3 ~a:[a_id "pkgs-added"] [txt "System packages added"];
      code (key_values added_pkgs);
      h3 ~a:[a_id "pkgs-removed"] [txt "System packages removed"];
      code (key_values removed_pkgs);
      h3 ~a:[a_id "pkgs-changed"] [txt "System packages changed"];
      code (key_value_changes changed_pkgs);
    ])

open Tyxml.Html

let txtf fmt = Fmt.kstrf txt fmt
let a_titlef fmt = Fmt.kstrf a_title fmt

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
    a ~a:[a_href (Fmt.strf "/job/%s/build/%a/f/%a"
                    job_name
                    Uuidm.pp build.Builder_db.Build.Meta.uuid
                    Fpath.pp filepath)]
      [if basename
       then txt (Fpath.basename filepath)
       else txtf "%a" Fpath.pp filepath];
    txtf " (%a) " Fmt.byte_size size;
    code [txtf "SHA256:%a" Hex.pp (Hex.of_cstruct sha256)];
  ]



let builder jobs =
  layout ~title:"Builder Web"
    [ h1 [txt "Builder web"];
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
      p [
        txtf "We have currently %d jobs."
          (List.length jobs);
      ];
      ul (List.map (fun (job_name, latest_build, latest_artifact) ->
          li ([
              a ~a:[a_href ("job/" ^ job_name ^ "/")]
                [txt job_name];
              txt " ";
              check_icon latest_build.Builder_db.Build.Meta.result;
              br ();
              a ~a:[a_href (Fmt.strf "job/%s/build/%a/" job_name Uuidm.pp
                              latest_build.Builder_db.Build.Meta.uuid)]
                [txtf "%a" (Ptime.pp_human ()) latest_build.Builder_db.Build.Meta.start];
              txt " ";
            ] @ match latest_artifact with
            | Some main_binary ->
              artifact ~basename:true job_name latest_build main_binary
            | None ->
              [
                txtf "Build failed";
              ]))
        jobs);
    ]

let job name builds =
  layout ~title:(Printf.sprintf "Job %s" name)
    [ h1 [txtf "Job %s" name];
      p [
        txtf "Currently %d builds."
          (List.length builds)
      ];
      ul (List.map (fun (build, main_binary) ->
          li ([
              a ~a:[a_href Fpath.(to_string (v "build" / Uuidm.to_string build.Builder_db.Build.Meta.uuid / ""))]
                [
                  txtf "%a" (Ptime.pp_human ()) build.Builder_db.Build.Meta.start;
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

    ]

let job_build
  name
  { Builder_db.Build.uuid = _; start; finish; result; console; script; main_binary = _; job_id = _ }
  artifacts
  =
  let ptime_pp = Ptime.pp_human () in
  let delta = Ptime.diff finish start in
  layout ~title:(Fmt.strf "Job build %s %a" name ptime_pp start)
    [ h1 [txtf "Job build %s %a" name ptime_pp start];
      p [txtf "Build took %a." Ptime.Span.pp delta ];
      p [txtf "Execution result: %a." Builder.pp_execution_result result];
      h3 [txt "Digests of build artifacts"];
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
    ]

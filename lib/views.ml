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



let builder jobs =
  layout ~title:"Builder Web"
    [ h1 [txt "Builder web"];
      p [
        txtf "We have currently %d jobs."
          (List.length jobs);
      ];
      ul (List.map (fun job ->
          li [
            a ~a:[a_href ("job/" ^ job ^ "/")]
              [txt job];
          ])
          jobs);
    ]

let job name builds =
  layout ~title:(Printf.sprintf "Job %s" name)
    [ h1 [txtf "Job %s" name];
      p [
        txtf "Currently %d builds."
          (List.length builds)
      ];
      ul (List.map (fun build ->
          li [
            a ~a:[a_href Fpath.(to_string (v "build" / Uuidm.to_string build.Builder_db.Build.Meta.uuid) ^ "/")]
              [
                txtf "%a" (Ptime.pp_human ()) build.Builder_db.Build.Meta.start;
              ];
            txt " ";
            check_icon build.result;
          ])
          builds);

    ]

let job_build
  name
  { Builder_db.Build.uuid = _; start; finish; result; console; script; job_id = _ }
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
            (fun { Builder_db.filepath; localpath=_; sha256; } ->
               let (`Hex sha256_hex) = Hex.of_cstruct sha256 in
               [
                 dt [a
                       ~a:[Fmt.kstr a_href "f/%a" Fpath.pp filepath]
                       [code [txtf "%a" Fpath.pp filepath]];
                     txt "(SHA256)"];
                 dd [code [txt sha256_hex]];
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

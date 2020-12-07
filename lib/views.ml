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
            txt ".output-ts { white-space: nowrap; }";
            txt ".output-code { overflow: visible; }"
          ]])
    (body body_)

let builder jobs =
  layout ~title:"Builder Web"
    [ h1 [txt "Builder web"];
      p [
        txtf "We have currently %d jobs."
          (List.length jobs);
      ];
      ul (List.map (fun job ->
          li [
            a ~a:[a_href ("job/" ^ Fpath.to_string job ^ "/")]
              [txt (Fpath.to_string job)];
          ])
          jobs);
    ]

let job name runs =
  layout ~title:(Printf.sprintf "Job %s" name)
    [ h1 [txtf "Job %s" name];
      p [
        txtf "Currently %d builds."
          (List.length runs)
      ];
      ul (List.map (fun run ->
          li [
            a ~a:[a_href Fpath.(to_string (v "run" / Uuidm.to_string run.Model.uuid) ^ "/")]
              [
                txtf "%a" (Ptime.pp_human ()) run.Model.start;
              ];
            txt " ";
            check_icon run.result;
          ])
          runs);

    ]

let job_run
    { Model.meta = {
          Model.job_info = { Builder.name; _ };
          start; finish; uuid = _; result };
      out; data = _ }
  =
  let ptime_pp = Ptime.pp_human () in
  let delta = Ptime.diff finish start in
  layout ~title:(Fmt.strf "Job build %s %a" name ptime_pp start)
    [ h1 [txtf "Job build %s %a" name ptime_pp start];
      p [txtf "Build took %a." Ptime.Span.pp delta ];
      p [txtf "Execution result: %a." Builder.pp_execution_result result];
      table (List.concat_map (fun (ts, line) ->
          [tr [
              td ~a:[a_class ["output-ts"]]
                [txtf "%d ms" (Duration.to_ms (Int64.of_int ts))];
              td ~a:[a_class ["output-code"]]
                [code [txt line]];
            ]])
          (List.rev out));
    ]

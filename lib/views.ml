open Tyxml.Html

let txtf fmt = Fmt.kstrf txt fmt

let check_icon check =
  if check
  then
    span ~a:[a_style "color: green;"]
      [txt "☑"]
  else
    span ~a:[a_style "color: red;"]
      [txt "☒"]

let layout ~title:title_ body_ =
  html
    (head (title (txt title_))
       [])
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
            a ~a:[a_href ("job/" ^ Model.job_name job ^ "/")]
              [txt (Model.job_name job)];
          ])
          jobs);
    ]

let job name runs =
  layout ~title:(Printf.sprintf "Job %s" name)
    [ h1 [txtf "Job %s" name];
      p [
        txtf "Currently %d job runs."
          (List.length runs)
      ];
      ul (List.map (fun run ->
          li [
            a ~a:[a_href Fpath.(to_string (v "run" / Uuidm.to_string run.Model.uuid) ^ "/")]
              [
                txtf "%a" (Ptime.pp_human ()) run.Model.start;
                check_icon (match run.result with Builder.Exited 0 -> true | _ -> false);
              ];
          ])
          runs);

    ]

let job_run
    { Model.meta = {
          Model.job_info = { Builder.name; _ };
          start; finish; uuid; result };
      out; _ }
  =
  let ptime_pp = Ptime.pp_human () in
  let delta = Ptime.diff finish start in
  layout ~title:(Printf.sprintf "Job run %s (%s)" name (Uuidm.to_string uuid))
    [ h1 [txtf "Job build %s %a (%a)" name ptime_pp start Uuidm.pp uuid];
      p [txtf "Took %a." Ptime.Span.pp delta ];
      p [txtf "Status: %a." Builder.pp_execution_result result];
      div (List.concat_map (fun (ts, line) ->
          [
            code [txtf "%d ms %s" (Duration.to_ms (Int64.of_int ts)) line];
            br ();
          ])
          (List.rev out));
    ]

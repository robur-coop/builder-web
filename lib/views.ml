open Tyxml.Html

let txtf fmt = Fmt.kstrf txt fmt

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

let job job =
  let name = Model.job_name job in
  layout ~title:(Printf.sprintf "Job %s" name)
    [ h1 [txtf "Job %s" name];
      p [
        txtf "Currently %d job runs."
          (List.length job.Model.runs)
      ];
      ul (List.map (fun (run : Fpath.t) ->
          li [
            a ~a:[a_href Fpath.(to_string (v "run" // run) ^ "/")]
              [txtf "%a" Fpath.pp run];
          ])
          job.Model.runs);

    ]

let job_run { Model.job_info = { Builder.name; _ };
              uuid; result; out; _ } =
  layout ~title:(Printf.sprintf "Job run %s (%s)" name (Uuidm.to_string uuid))
    [ h1 [txtf "Job build %s (%a)" name Uuidm.pp uuid];
      p [txtf "Status: %a" Builder.pp_execution_result result];
      div (List.concat_map (fun (ts, line) ->
          [
            code [txtf "%d ms %s" (Duration.to_ms (Int64.of_int ts)) line];
            br ();
          ])
          (List.rev out));
    ]

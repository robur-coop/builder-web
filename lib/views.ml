open Tyxml.Html

let layout ~title:title_ body_ =
  html
    (head (title (txt title_))
       [])
    (body body_)

let builder jobs =
  layout ~title:"Builder Web"
    [ h1 [txt "Builder web"];
      p [
        txt "We have currently ";
        txt (string_of_int (List.length jobs));
        txt " jobs.";
      ];
      ul (List.map (fun job ->
          li [txt (Model.job_name job)])
          jobs);
    ]

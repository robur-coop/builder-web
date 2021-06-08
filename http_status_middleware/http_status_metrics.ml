open Lwt.Infix

let create ~f =
  let data : (string, int) Hashtbl.t = Hashtbl.create 7 in
  (fun x ->
     let key = f x in
     let cur = match Hashtbl.find_opt data key with
       | None -> 0
       | Some x -> x
     in
     Hashtbl.replace data key (succ cur)),
  (fun () ->
     let data, total =
       Hashtbl.fold (fun key value (acc, total) ->
           (Metrics.uint key value :: acc), value + total)
         data ([], 0)
     in
     Metrics.uint "total" total :: data)

let counter_metrics ~f name =
  let open Metrics in
  let doc = "Counter metrics" in
  let incr, get = create ~f in
  let data thing = incr thing; Data.v (get ()) in
  Src.v ~doc ~tags:Metrics.Tags.[] ~data name

let add_http_status =
  let f = function
    | #Dream.informational -> "1xx"
    | #Dream.successful -> "2xx"
    | #Dream.redirection -> "3xx"
    | #Dream.client_error -> "4xx"
    | #Dream.server_error -> "5xx"
    | `Status c -> Printf.sprintf "%dxx" (c / 100)
  in
  let src = counter_metrics ~f "http_response" in
  (fun r -> Metrics.add src (fun x -> x) (fun d -> d r))

let handle next_handler req =
  next_handler req >|= fun response ->
  add_http_status (Dream.status response);
  response

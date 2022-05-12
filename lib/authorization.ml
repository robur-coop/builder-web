
let src = Logs.Src.create "authorization" ~doc:"Builder_web authorization"
module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Syntax

let realm = "builder-web"

let user_info_field = Dream.new_field ~name:"user_info" ()

let authenticate handler = fun req ->
  let unauthorized () =
    let headers = ["WWW-Authenticate", Printf.sprintf "Basic realm=\"%s\"" realm] in
    Dream.respond ~headers ~status:`Unauthorized "Forbidden!"
  in
  match Dream.header req "Authorization" with
  | None -> unauthorized ()
  | Some data -> match String.split_on_char ' ' data with
     | [ "Basic" ; user_pass ] ->
       (match Base64.decode user_pass with
       | Error `Msg msg ->
         Log.info (fun m -> m "Invalid user / pasword encoding in %S: %S" data msg);
         Dream.respond ~status:`Bad_Request "Couldn't decode authorization"
       | Ok user_pass -> match String.split_on_char ':' user_pass with
         | [] | [_] ->
           Log.info (fun m -> m "Invalid user / pasword encoding in %S" data);
           Dream.respond ~status:`Bad_Request "Couldn't decode authorization"
         | user :: password ->
           let pass = String.concat ":" password in
           let* user_info = Dream.sql req (Model.user user) in
           match user_info with
           | Ok (Some (id, user_info)) ->
             if Builder_web_auth.verify_password pass user_info
             then (Dream.set_field req user_info_field (id, user_info); handler req)
             else unauthorized ()
           | Ok None ->
             let _ : _ Builder_web_auth.user_info =
               Builder_web_auth.hash ~username:user ~password:pass ~restricted:true () in
             unauthorized ()
           | Error e ->
             Log.warn (fun m -> m "Error getting user: %a" Model.pp_error e);
             Dream.respond ~status:`Internal_Server_Error "Internal server error")
     | _ ->
       Log.warn (fun m -> m "Error retrieving authorization %S" data);
       Dream.respond ~status:`Bad_Request "Couldn't decode authorization"

let authorized req job_name =
  match Dream.field req user_info_field with
  | None -> Lwt.return (Error (`Msg "not authenticated"))
  | Some (id, user) ->
     if user.restricted then
       Dream.sql req (Model.authorized id job_name)
     else
       Lwt_result.return ()

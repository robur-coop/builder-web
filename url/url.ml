let src = Logs.Src.create "builder-web.url" ~doc:"Builder-web URL"
module Log = (val Logs.src_log src : Logs.LOG)

open Vif.Uri

let uuid =
  Vif.Uri.conv
    (fun s -> match (Uuidm.of_string s) with
       | None -> Log.err (fun m -> m "bad uuid %S" s); invalid_arg "Uuidm.of_string"
       | Some uuid -> uuid)
    (Uuidm.to_string ~upper:false)
    (Vif.Uri.string `Path)
let script_or_console =
  Tyre.(str "script" <|> str "console")
  |> Tyre.conv (function `Left () -> `Script | `Right () -> `Console)
    (function `Script -> `Left () | `Console -> `Right ())
let viz =
  Tyre.(str "viztreemap" <|> str "vizdependencies")
  |> Tyre.conv (function `Left () -> `Treemap | `Right () -> `Dependencies)
    (function `Treemap -> `Left () | `Dependencies -> `Right ())
let hex =
  Tyre.regex Re.(rep (seq [xdigit; xdigit]))
  |> Vif.Uri.conv (Ohex.decode ~skip_whitespace:false)
    Ohex.encode

(* All URIs are thunked ([unit -> _ Vif_uri.t]) so we don't fall into value restriction *)
let root () = rel /?? nil
let all_builds () = rel / "all-builds" /?? nil
let failed_builds () = rel / "failed-builds" /?? any

let prefix_job () = rel / "job" /% string `Path
let job () = prefix_job () /?? any
let job_with_failed () = prefix_job () / "failed" /?? any

let redirect_latest () = prefix_job () / "build" / "latest" /% path /?? any
let redirect_latest_empty () = prefix_job () / "build" / "latest" /?? any
let job_build () = prefix_job () / "build" /% uuid /?? any
let job_build_file () = prefix_job () / "build" /% uuid / "f" /% path /?? any
let job_build_static_file () = prefix_job () / "build" /% uuid /% script_or_console /?? any
let job_build_viz () = prefix_job () / "build" /% uuid /% viz /?? any
let redirect_main_binary () = prefix_job () / "build" /% uuid / "main-binary" /?? any
let exec () = prefix_job () / "build" /% uuid / "exec" /?? any
let upload_binary () = prefix_job () / "platform" /% string `Path / "upload" /?? any

let compare_builds () = rel / "compare" /% uuid /% uuid /?? nil

let hash () = rel / "hash" /?? ("sha256", hex) ** any

let robots () = rel / "robots.txt" /?? any

let upload () = rel / "upload" /?? nil

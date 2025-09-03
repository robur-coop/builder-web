let fpath_url_pp ppf f =
  Fpath.segs f
  |> List.map Uri.pct_encode
  |> Fmt.(list ~sep:(any "/") string) ppf

module Queries_aux = struct

  let flatten = Option.value ~default:[]
  
  let add_raw url_str queries =
    let uri = Uri.of_string url_str in
    let uri = Uri.add_query_params uri queries in
    Uri.to_string uri

  let add ~encode_query queries url_str =
    queries |> flatten |> List.map encode_query |> add_raw url_str
  
end

let pctencode fmt str = Format.fprintf fmt "%s" (Uri.pct_encode str)

module Root = struct

  let make () = "/"

end

module Job = struct

  let encode_query = function
    | `Platform p -> "platform", [ p ]

  let make ?queries ~job_name () =
    Fmt.str "/job/%a" pctencode job_name
    |> Queries_aux.add ~encode_query queries

  let make_failed ?queries ~job_name () =
    Fmt.str "/job/%a/failed" pctencode job_name
    |> Queries_aux.add ~encode_query queries
  
end

module Job_build = struct

  let make ~job_name ~build () =
    Fmt.str "/job/%a/build/%a"
      pctencode job_name
      Uuidm.pp build

end

module Job_build_artifact = struct

  let encode_artifact = function
    | `Main_binary -> "/main-binary"
    | `Viz_treemap -> "/viztreemap"
    | `Viz_dependencies -> "/vizdependencies"
    | `Script -> "/script"
    | `Console -> "/console"
    | `All_targz -> "/all.tar.gz"
    | `File f -> "/f/" ^ Fmt.to_to_string fpath_url_pp f

  let make_from_string ~job_name ~build ~artifact () =
    Fmt.str "/job/%a/build/%a/%s"
      pctencode job_name
      Uuidm.pp build
      artifact
  
  let make ~job_name ~build ~artifact () =
    let artifact = encode_artifact artifact in
    make_from_string ~job_name ~build ~artifact ()

end

module Compare_builds = struct

  let make ~left ~right () =
    Fmt.str "/compare/%a/%a"
      Uuidm.pp left
      Uuidm.pp right
  
end

module Failed_builds = struct

  let make ~count ~start () =
    Fmt.str "/failed-builds?count=%d&start=%d" count start

end

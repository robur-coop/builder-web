module String_map = struct
  include Map.Make(String)

  let add_or_create key v t=
    update key (function None -> Some [ v ] | Some xs -> Some (v :: xs)) t
end

let diff_map_to_json (left, right, different_versions) =
  let convert_list lst =
    `List (List.map (fun (name, version) ->
      `Assoc [("name", `String name); ("version", `String version)]
    ) lst)
  in
  let convert_diff_versions lst =
    `List (List.map (fun (name, version1, version2) ->
      `Assoc [
        ("name", `String name);
        ("version_left", `String version1);
        ("version_right", `String version2)
      ]
    ) lst)
  in
  `Assoc [
    ("left_packages", convert_list left);
    ("right_packages", convert_list right);
    ("different_versions", convert_diff_versions different_versions)
  ]

let diff_map a b =
  let diff a b =
    String_map.fold (fun k v acc ->
      if not (String_map.mem k b) then (k, v) :: acc else acc)
      a [] |> List.rev
  in
  let added = diff b a
  and removed = diff a b
  and changed =
    String_map.fold (fun k v acc ->
      match String_map.find_opt k b with
      | None -> acc
      | Some v' -> if String.equal v v' then acc else (k, v, v') :: acc)
    a [] |> List.rev
  in
  (added, removed, changed)

let compare_env env1 env2 =
  let parse_env e =
    List.fold_left (fun m s ->
      match Astring.String.cut ~sep:"=" s with
      | Some (key, value) -> String_map.add key value m
      | None -> String_map.add s "" m)
    String_map.empty (Astring.String.cuts ~sep:"\n" e)
  in
  diff_map (parse_env env1) (parse_env env2)

let compare_pkgs p1 p2 =
  let parse_pkgs p =
    List.fold_left (fun m s ->
      match Astring.String.cut ~sep:"=" s with
      | Some (name, version) -> String_map.add name version m
      | None -> match Astring.String.cut ~sep:"-" s with
        | Some (name, version) -> String_map.add name version m
        | None -> String_map.add s "" m)
    String_map.empty (Astring.String.cuts ~sep:"\n" p)
  in
  diff_map (parse_pkgs p1) (parse_pkgs p2)

let md_to_html ?adjust_heading ?(safe = true) data =
  let open Cmarkit in
  let doc = Doc.of_string ~strict:false ~heading_auto_ids:true data in
  let doc =
    Option.fold ~none:doc
      ~some:(fun lvl ->
          let block _m = function
            | Block.Heading (h, meta) ->
              let open Block.Heading in
              let level = level h
              and id = id h
              and layout = layout h
              and inline = inline h
              in
              let h' = make ?id ~layout ~level:(level + lvl) inline in
              Mapper.ret (Block.Heading (h', meta))
            | Block.Blocks _ -> Mapper.default
            | x -> Mapper.ret x
          in
          let mapper = Mapper.make ~block () in
          Mapper.map_doc mapper doc)
      adjust_heading
  in
  Cmarkit_html.of_doc ~safe doc

module Path = struct

  let to_url ~path ~queries =
    let path = match path with
      | "" :: [] -> "/"
      | path -> "/" ^ String.concat "/" path
    in
    let query = queries |> List.map (fun (k, v) -> k, [v]) in
    Uri.make ~path ~query () |> Uri.to_string

  (* Like Dream.path in 1.0.0~alpha2 but on Dream.target *)
  let of_url uri_str =
    let path_str = uri_str |> Uri.of_string |> Uri.path in
    match String.split_on_char '/' path_str with
    | "" :: (_ :: _ as tail) -> tail
    | path -> path

  let matches_dreamroute ~path dreamroute =
    let is_match path_elem dpath_elem =
      (dpath_elem |> String.starts_with ~prefix:":")
      || path_elem = dpath_elem
    in
    let rec aux path dreampath =
      match path, dreampath with
      | []     , _ :: _   -> false   (*length path < length dreampath*)
      | _      , []       -> true    (*length path >= length dreampath *)
      | _ :: _ , "" :: [] -> true    (*dreampath ends in '/'*)
      | p_elem :: path, dp_elem :: dreampath ->
        is_match p_elem dp_elem
        && aux path dreampath
    in
    let dreampath = dreamroute |> of_url in
    aux path dreampath

end

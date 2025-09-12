let ( let* ) = Result.bind

let viz_type_to_string = function
  | `Treemap -> "treemap"
  | `Dependencies -> "dependencies"

let viz_dir ~cachedir ~viz_typ ~version =
  let typ_str = viz_type_to_string viz_typ in
  Fpath.(cachedir / Fmt.str "%s_%d" typ_str version)

let viz_path ~cachedir ~viz_typ ~version ~input_hash =
  Fpath.( viz_dir ~cachedir ~viz_typ ~version / input_hash + "html")

let choose_versioned_viz_path
    ~cachedir
    ~viz_typ
    ~viz_input_hash
    ~current_version =
  let ( >>= ) = Result.bind in
  let rec aux current_version =
    let path =
      viz_path ~cachedir
        ~viz_typ
        ~version:current_version
        ~input_hash:viz_input_hash in
    Bos.OS.File.exists path >>= fun path_exists ->
    if path_exists then Ok path else (
      if current_version = 1 then
        Error (`Msg (Fmt.str "viz '%s': There exist no version of the requested \
                              visualization"
                       (viz_type_to_string viz_typ)))
      else
        aux @@ pred current_version
    )
  in
  aux current_version

let get_viz_version_from_dirs ~cachedir ~viz_typ =
  let* versioned_dirs = Bos.OS.Dir.contents cachedir in
  let max_cached_version =
    let viz_typ_str = viz_type_to_string viz_typ ^ "_" in
    versioned_dirs
    |> List.filter_map (fun versioned_dir ->
        match Bos.OS.Dir.exists versioned_dir with
        | Error (`Msg err) ->
          Logs.warn (fun m -> m "%s" err);
          None
        | Ok false -> None
        | Ok true ->
          let dir_str = Fpath.filename versioned_dir in
          if not (String.starts_with ~prefix:viz_typ_str dir_str) then
            None
          else
            try
              String.(sub dir_str
                        (length viz_typ_str)
                        (length dir_str - length viz_typ_str))
              |> int_of_string
              |> Option.some
            with Failure _ ->
              Logs.warn (fun m ->
                  m "Failed to read visualization-version from directory: '%s'"
                    (Fpath.to_string versioned_dir));
              None
      )
    |> List.fold_left Int.max (-1)
  in
  if max_cached_version = -1 then
    Result.error @@
    `Msg (Fmt.str "Couldn't find any visualization-version of %s"
            (viz_type_to_string viz_typ))
  else
    Result.ok max_cached_version

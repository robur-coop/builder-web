module Set = OpamPackage.Set

let packages (switch : OpamFile.SwitchExport.t) =
  assert (Set.cardinal switch.selections.sel_pinned = 0);
  assert (Set.cardinal switch.selections.sel_compiler = 0);
  assert (Set.subset switch.selections.sel_roots switch.selections.sel_installed);
  switch.selections.sel_installed

let duniverse_dir = "x-opam-monorepo-duniverse-dirs"

module M = Map.Make(String)

let duniverse_dirs_data =
  (* the representation in the file is [ URL DIR [ HASH* ] ] *)
  let open OpamParserTypes.FullPos in
  let ( let* ) = Result.bind in
  let string ~ctx = function
    | { pelem = String s ; _ } -> Ok s
    | _ -> Error (`Msg ("couldn't find a string " ^ ctx))
  in
  let extract_data = function
    | { pelem = List { pelem = [ url ; dir ; hashes ] ; _ } ; _ } ->
      let* url = string ~ctx:"url" url in
      let* hashes =
        match hashes with
        | { pelem = List { pelem = hashes ; _ } ; _ } ->
          List.fold_left (fun acc hash ->
              let* acc = acc in
              let* hash = string ~ctx:"hash" hash in
              let* h = match OpamHash.of_string_opt hash with
                | Some h -> Ok OpamHash.(kind h, contents h)
                | None -> Error (`Msg ("couldn't decode opam hash in " ^ hash))
              in
              Ok (h :: acc))
            (Ok []) hashes
        | _ -> Error (`Msg "couldn't decode hashes")
      in
      let* dir = string ~ctx:"directory" dir in
      Ok (url, dir, List.rev hashes)
    | { pelem = List { pelem = [ url ; dir ] ; _ } ; _ } ->
      let* url = string ~ctx:"url" url in
      let* dir = string ~ctx:"directory" dir in
      Ok (url, dir, [])
    | _ -> Error (`Msg "expected a list of URL, DIR, [HASHES]")
  in
  function
  | { pelem = List { pelem = lbody ; _ } ; _ } ->
    List.fold_left (fun acc v ->
        let* acc = acc in
        let* (url, dir, hashes) = extract_data v in
        Ok (M.add dir (url, hashes) acc))
      (Ok M.empty) lbody
  | _ -> Error (`Msg "expected a list or a nested list")

let duniverse (switch : OpamFile.SwitchExport.t) =
  let root = switch.OpamFile.SwitchExport.selections.OpamTypes.sel_roots in
  if OpamPackage.Set.cardinal root = 1 then
    let root = OpamPackage.Set.choose root in
    match OpamPackage.(Name.Map.find_opt root.name switch.OpamFile.SwitchExport.overlays) with
    | None -> Error (`Msg "opam switch export doesn't contain the main package")
    | Some opam ->
      match OpamFile.OPAM.extended opam duniverse_dir duniverse_dirs_data with
      | None -> Ok None
      | Some Error e -> Error e
      | Some Ok v -> Ok (Some v)
  else
    Error (`Msg "not a single root package found in opam switch export")

type duniverse_diff = {
  name : string ;
  urls : string * string option ;
  hash : (OpamHash.kind * string option * string option) list ;
}

let pp_duniverse_diff ppf { name ; urls ; hash } =
  let opt_hash = Option.value ~default:"NONE" in
  Format.fprintf ppf "%s (%s%s) %s"
    name
    (fst urls)
    (Option.fold ~none:"" ~some:(fun url -> "->" ^ url) (snd urls))
    (String.concat ", " (List.map (fun (h, l, r) ->
         OpamHash.string_of_kind h ^ " " ^ opt_hash l ^ "->" ^ opt_hash r) hash))

let pp_duniverse_dir ppf (dir, url) =
  Format.fprintf ppf "%s (%s)" dir url

let duniverse_diff l r =
  let l = Option.value l ~default:M.empty
  and r = Option.value r ~default:M.empty
  in
  let keys_l_only = ref [] and keys_r_only = ref [] and diff = ref [] in
  let equal_hashes l r =
    (* l and r are lists of pairs, with the hash kind and its value *)
    (* for a git remote, the hashes are empty lists *)
    (match l with [] -> false | _ -> true) &&
    (match r with [] -> false | _ -> true) &&
    List.for_all (fun (h, v) ->
        match List.assoc_opt h r with
        | None -> false
        | Some v' -> String.equal v v')
      l &&
    List.for_all (fun (h, v) ->
        match List.assoc_opt h l with
        | None -> false
        | Some v' -> String.equal v v')
      r
  in
  let _ =
    M.merge (fun key l r ->
        match l, r with
        | None, Some _ -> keys_r_only := key :: !keys_r_only; None
        | Some _, None -> keys_l_only := key :: !keys_l_only; None
        | None, None -> None
        | Some (_, l), Some (_, r) when equal_hashes l r -> None
        | Some (url1, []), Some (url2, []) when String.equal url1 url2 -> None
        | Some l, Some r -> diff := (key, l, r) :: !diff; None)
      l r
  in
  let dir_only keys map =
    let only =
      M.filter (fun k _ -> List.mem k keys) map |> M.bindings
    in
    List.map (fun (key, (url, _)) -> key, url) only
  in
  let l_only = dir_only !keys_l_only l
  and r_only = dir_only !keys_r_only r
  and diff =
    List.map (fun (name, (url_l, hashes_l), (url_r, hashes_r)) ->
        let urls =
          if String.equal url_l url_r then url_l, None else url_l, Some url_r
        in
        let hash =
          List.fold_left (fun acc (h, v) ->
              match List.assoc_opt h hashes_r with
              | None -> (h, Some v, None) :: acc
              | Some v' ->
                if String.equal v v' then
                  acc
                else
                  (h, Some v, Some v') :: acc)
            [] hashes_l
        in
        let hash = List.fold_left (fun acc (h', v') ->
            match List.assoc_opt h' hashes_l with
            | None -> (h', None, Some v') :: acc
            | Some _ -> acc)
            hash hashes_r
        in
        { name ; urls ; hash })
      !diff
  in
  l_only, r_only, diff

type version_diff = {
  name : OpamPackage.Name.t;
  version_left : OpamPackage.Version.t;
  version_right : OpamPackage.Version.t;
}

let pp_opampackage ppf p =
  Format.fprintf ppf "%s" (OpamPackage.to_string p)

let pp_version_diff ppf { name; version_left; version_right } =
  Format.fprintf ppf "%s.%s->%s"
    (OpamPackage.Name.to_string name)
    (OpamPackage.Version.to_string version_left)
    (OpamPackage.Version.to_string version_right)

type opam_diff = {
  pkg : OpamPackage.t ;
  effectively_equal : bool ;
  diff : string ;
}

let pp_opam_diff ppf { pkg ; effectively_equal ; _ } =
  Format.fprintf ppf "%a%s"
    pp_opampackage pkg
    (if effectively_equal then "" else " (effectively equal)")

let detailed_opam_diff pkg l r =
  let opaml = OpamFile.OPAM.write_to_string l in
  let opamr =
    (* Let's minimize the difference between opaml and opamr by taking opaml
       as template for opamr. *)
    let o = OpamFile.make (OpamFilename.raw "opam") in
    OpamFile.OPAM.to_string_with_preserved_format ~format_from_string:opaml o r
  in
  let effectively_equal =
    let no_build_install_url p =
      OpamFile.OPAM.with_url_opt None
        (OpamFile.OPAM.with_install []
           (OpamFile.OPAM.with_build [] p))
    in
    OpamFile.OPAM.effectively_equal
      (no_build_install_url l) (no_build_install_url r)
  in
  let diff =
    let label_l =
      Printf.sprintf "left/%s/opam" (OpamPackage.name_to_string pkg)
    and label_r =
      Printf.sprintf "right/%s/opam" (OpamPackage.name_to_string pkg)
    in
    try
      Bos.OS.File.with_tmp_oc "opaml_%s"
        (fun pl oc () ->
           Out_channel.output_string oc opaml;
           Out_channel.close oc;
           Bos.OS.File.with_tmp_oc "opamr_%s"
             (fun pr oc () ->
                Out_channel.output_string oc opamr;
                Out_channel.close oc;
                let cmd =
                  Bos.Cmd.(v "diff" % "-u" % "--label" % label_l % "--label" % label_r %
                           p pl % p pr)
                in
                Bos.OS.Cmd.(run_out cmd |> out_string))
        ())
        ()
    with e ->
      Error (`Msg ("exception " ^ Printexc.to_string e))
  in
  let diff = match diff with
    | Ok (Ok (Ok (data, _))) -> data
    | Ok (Ok (Error `Msg msg))
    | Ok (Error `Msg msg)
    | Error `Msg msg ->
      Logs.err (fun m -> m "Error %s while running diff on opam files@.@.%s@.@.%s@.@."
                   msg opaml opamr);
      "Error comparing opam files"
  in
  { pkg ; effectively_equal ; diff }

let detailed_opam_diffs left right pkgs =
  OpamPackage.Set.fold (fun p acc ->
      let find = OpamPackage.Name.Map.find p.name in
      let opam_left = find left.OpamFile.SwitchExport.overlays
      and opam_right = find right.OpamFile.SwitchExport.overlays in
      (detailed_opam_diff p opam_left opam_right) :: acc)
    pkgs []

let compare left right =
  let packages_left = packages left and packages_right = packages right in
  let module Set = OpamPackage.Set in
  let equal_name p1 p2 = OpamPackage.Name.equal p1.OpamPackage.name p2.OpamPackage.name in
  let diff l r =
    Set.filter (fun p1 ->
        not (Set.exists (equal_name p1) r))
      l
  in
  let same_version = Set.inter packages_left packages_right in
  let opam_diff =
    Set.filter
      (fun p ->
         let find = OpamPackage.Name.Map.find p.name in
         let opam_left = find left.overlays and opam_right = find right.overlays in
         not (OpamFile.OPAM.effectively_equal opam_left opam_right))
      same_version
  and version_diff =
    List.filter_map (fun p1 ->
        match Set.find_opt (equal_name p1) packages_right with
        | Some p2 ->
          if OpamPackage.Version.equal p1.version p2.version
          then None
          else
            Some { name = p1.OpamPackage.name;
                   version_left = p1.OpamPackage.version;
                   version_right = p2.OpamPackage.version }
        | None ->
          None)
      (Set.elements packages_left)
  and left_pkgs = diff packages_left packages_right
  and right_pkgs = diff packages_right packages_left
  in
  let opam_diff = detailed_opam_diffs left right opam_diff in
  let duniverse_ret =
    match duniverse left, duniverse right with
    | Ok l, Ok r -> Ok (duniverse_diff l r)
    | Error _ as e, _ | _, (Error _ as e) -> e
  in
  (opam_diff, version_diff, left_pkgs, right_pkgs, duniverse_ret)

let compare_to_json
  (opam_diff, version_diff, left_pkgs, right_pkgs, duniverse_diff) : Yojson.Basic.t =
    let version_diff_to_json lst =
      `List (List.map (fun { name; version_left; version_right } ->
          `Assoc [
            ("name", `String (OpamPackage.Name.to_string name));
            ("version_left", `String (OpamPackage.Version.to_string version_left));
            ("version_right", `String (OpamPackage.Version.to_string version_right))
          ]) lst)
    in
    let package_set_to_json set =
      `List (Set.fold (fun p acc ->
          let json = `Assoc [
              ("name", `String (OpamPackage.Name.to_string p.OpamPackage.name));
              ("version", `String (OpamPackage.Version.to_string p.OpamPackage.version))
            ] in
          json :: acc) set [])
    in
    let opam_diff_to_json opam_diff =
      `List (List.map (fun (diff : opam_diff) ->
        `Assoc [
          ("package_version", `String (OpamPackage.to_string diff.pkg));
          ("effectively_equal", `Bool diff.effectively_equal);
          ("diff", `String diff.diff);
        ]) opam_diff)
    in
    let duniverse_to_json = function
      | Ok (left, right, detailed_diff) ->
        `Assoc [
          ("left", `List (List.map (fun (k, v) -> `Assoc [("name", `String k); ("value", `String v)]) left));
          ("right", `List (List.map (fun (k, v) -> `Assoc [("name", `String k); ("value", `String v)]) right));
          ("detailed_diff",`List (List.map (fun (diff : duniverse_diff) ->
               `Assoc [
                 ("name", `String diff.name);
               ]) detailed_diff))
        ]
      | Error (`Msg msg) ->
        `String msg
    in
    `Assoc [
      ("opam_diff", opam_diff_to_json opam_diff);
      ("version_diff", version_diff_to_json version_diff);
      ("only_in_left", package_set_to_json left_pkgs);
      ("only_in_right", package_set_to_json right_pkgs);
      ("duniverse_diff", duniverse_to_json duniverse_diff)
    ]

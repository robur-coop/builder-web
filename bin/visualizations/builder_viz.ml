let src = Logs.Src.create "builder-viz" ~doc:"Builder_viz"
module Log = (val Logs.src_log src : Logs.LOG)

open Rresult

(* open Lwt.Syntax
 * open Lwt_result.Infix *)

let read_file file =
  try
    let fh = open_in file in
    try
      let content = really_input_string fh (in_channel_length fh) in
      close_in_noerr fh ;
      content
    with _ ->
      close_in_noerr fh;
      invalid_arg ("Error reading file: " ^ file)
  with _ -> invalid_arg ("Error opening file " ^ file)

let print_treemap_html elf_path elf_size =
  let open Modulectomy in
  let infos = 
    elf_path
    |> Elf.get
    |> Result.map_error (fun _ -> R.msg "Invalid ELF file")
    |> R.failwith_error_msg
  in
  let info, excluded_minors =
    let size, info =
      infos
      |> Info.import
      |> Info.diff_size_tree
    in
    (*> Note: this heuristic fails if one has all subtrees of equal size*)
    let node_big_enough subtree =
      match Info.(subtree.T.value.size) with
      | None -> true
      | Some subtree_size ->
        let pct = Int64.(to_float subtree_size /. to_float size) in
        pct > 0.004
    in
    info
    |> Info.prefix_filename
    |> Info.cut 2
    |> Info.partition_subtrees node_big_enough
  in
  let scale_chunks =
    let excluded_minors_size =
      excluded_minors
      |> List.map Info.compute_area
      |> List.fold_left Int64.add 0L
    in
    [ 
      "Smaller excluded entries", excluded_minors_size
    ]
  in
  let override_css = {|
    .module {
      fill: rgb(60, 60, 87);
    }
    .functor > text, .module > text {
      fill: bisque;
    }
  |}
  in
  info
  |> Treemap.of_tree
  |> Treemap.to_html_with_scale
    ~binary_size:elf_size
    ~scale_chunks
    ~override_css
  |> Tyxml.Html.pp () Format.std_formatter
(* |> Treemap.svg
 * |> Fmt.to_to_string (Tyxml.Svg.pp ()) *)

let print_dependencies_html file =
  let open Opam_graph in
  let switch = read_file file in
  let data = OpamFile.SwitchExport.read_from_string switch in
  let transitive = false in
  let graph = Ui.dependencies ~transitive data in
  let override_css = {|
    svg {
      background: rgb(60, 60, 87);
    }
  |}
  in
  let html = Render.Html.of_assoc ~override_css graph in
  Format.printf "%a" Render.Html.pp html

module Cmd = struct 

  module Arg = struct
  
    let elf_path =
      let doc = "The file-path of the debug-ELF to be analyzed" in
      Cmdliner.Arg.(
        required &
        pos 0 (some file) None &
        info ~doc ~docv:"DEBUG_ELF_PATH" []
      )

    let elf_size =
      let doc = "The file-size of the stripped ELF file in bytes" in
      Cmdliner.Arg.(
        required &
        pos 1 (some int) None &
        info ~doc ~docv:"STRIPPED_ELF_SIZE" []
      )

    let opam_switch_path =
      let doc = "The Opam-switch export file of the package to be analyzed" in
      Cmdliner.Arg.(
        required &
        pos 0 (some file) None &
        info ~doc ~docv:"SWITCH_EXPORT_PATH" []
      )

  end

  module Aux = struct

    let help man_format cmds = function
      | None -> `Help (man_format, None)
      | Some cmd ->
        if List.mem cmd cmds
        then `Help (man_format, Some cmd)
        else `Error (true, "Unknown command: " ^ cmd)

  end
  
  let treemap =
    let doc = "Dump treemap SVG and CSS wrapped in HTML" in
    Cmdliner.Term.(pure print_treemap_html $ Arg.elf_path $ Arg.elf_size),
    Cmdliner.Term.info ~doc "treemap"

  let dependencies =
    let doc = "Dump opam dependencies SVG and CSS wrapped in HTML" in
    Cmdliner.Term.(pure print_dependencies_html $ Arg.opam_switch_path),
    Cmdliner.Term.info ~doc "dependencies"

  let help =
    let topic =
      let doc = "Command to get help on" in
      Cmdliner.Arg.(value & pos 0 (some string) None & info ~doc ~docv:"COMMAND" [])
    in
    let doc = "Builder database help" in
    Cmdliner.Term.(ret (const Aux.help $ man_format $ choice_names $ topic)),
    Cmdliner.Term.info ~doc "help"

  let default =
    let doc = "Builder database command" in
    Cmdliner.Term.(ret (const Aux.help $ man_format $ choice_names $ const None)),
    Cmdliner.Term.info ~doc "builder-viz"

end

let () =
  Cmdliner.Term.eval_choice Cmd.default [
    Cmd.help;
    Cmd.treemap;
    Cmd.dependencies;
  ]
  |> Cmdliner.Term.exit

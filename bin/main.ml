exception SerializeError

open Yamlcv
(* open Yamlcv.Base_types *)

(***************************************************)
(**{1 Filter logic (filtering out)}*)
type filter_type = 
    | Only (* `Only tag will only include elements tagged with tag *)
    | Include (* `Include tag will only include elements not with no-tag. This is useful for languages*)
    | Exclude (* `Exclude tag will only include elements not tagged with tag *)

type filter = filter_type * Tags.tag

let apply_filter (filter, tag) tags = 
    let search_result =  Tags.TagMap.find_opt tag tags in
    match filter, search_result with
    | Only, (None|Some `Negative) -> false
    | Only, Some `Positive -> true
    | Include, (None|Some `Positive) -> true
    | Include, Some `Negative -> false
    | Exclude, (None|Some `Negative) -> true
    | Exclude, Some `Positive -> false

let apply_filters filters tags = List.for_all (fun filter -> apply_filter filter tags) filters

let filter_items filters = List.filter (fun (tags, _) -> apply_filters filters tags)


(***************************************************)
(**{1 Main code}*)
let main (wrap_in_ul:bool) (input_file:string) (output_file:string) (filters:filter list) = 
    let file_content = 
        match input_file with
        | "-" -> CCIO.read_all stdin
        | _ -> CCIO.(with_in input_file read_all) in
    let yaml = Yaml.of_string_exn file_content in
    let items = try serialize yaml with Failure s -> CCIO.write_line stderr s; raise SerializeError
    in
    let items = 
        items 
        |> filter_items filters
        |> List.map Html.tagged_item_to_html
    in 
    match wrap_in_ul with
    | false -> 
            let lines = 
            items 
            |> List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) 
            in
            begin match output_file with
            | "-" -> CCIO.write_lines_l stdout lines
            | _ -> CCIO.(with_out output_file CCIO.write_lines_l lines)
            end
    |true -> 
            let open Tyxml.Html in
            items 
            |> List.map (fun item -> li ~a:[a_class ["yamlcv"]] [item])
            |> ul ~a:[a_class ["yamlcv"]]
            |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
            |> match output_file with 
                | "-" -> CCIO.write_line stdout
                | output_file -> CCIO.(with_out output_file write_line)


(******************************
   {1 Cmdliner stuff}
*)

let cmd = 
    let open Cmdliner in
    let doc = "TODO" in
    let input_arg = Arg.(value & opt string "-" & 
    info ["i"; "input"] ~docv:"FILENAME" ~doc:"the yaml input file") in 
    let output_arg = Arg.(value & opt string "-" & 
    info ["o"; "output"] ~docv:"FILENAME" ~doc:"the output file") in
    let wrap_in_ul = Arg.(value & flag  & 
    info ["ul"]  ~doc:"whether to wrap the resulting divs in ul") in
    let filters =
        let only = fun tag -> Only, tag in
        let include_ = fun tag -> Include, tag in
        let exclude = fun tag -> Exclude, tag in
        let only_arg = Arg.(value & opt_all string [] & info ~docv:"TAG" ~doc:"will only include elements tagged with $(docv)" ["filter-only"] ) 
        and include_arg = Arg.(value & opt_all string [] & info ~docv:"TAG" ~doc:"will only include elements not tagged with no-$(docv). This is useful for languages" ["filter-include"] ) 
        and exclude_arg= Arg.(value & opt_all string [] & info ~docv:"TAG" ~doc:"will only include elements not tagged with $(docv)" ["filter-exclude"] ) 
        in
        let only_arg = Term.(const (List.map only) $ only_arg)
        and include_arg = Term.(const (List.map include_) $ include_arg)
        and exclude_arg = Term.(const (List.map exclude) $ exclude_arg)
        in 
        let merge_3 = fun a b c -> a @ b @ c in
        Term.(const merge_3 $ only_arg $ include_arg $ exclude_arg)
    in
    let term = Term.(const main $ wrap_in_ul $ input_arg $ output_arg $ filters) in
    let info = Cmd.info "yamlcv" ~doc in
    Cmd.v info term

let () = exit Cmdliner.(Cmd.eval cmd)


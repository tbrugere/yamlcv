exception SerializeError
exception Not_implemented

open Yamlcv
(* open Yamlcv.Base_types *)

(***************************************************)
(**{1 Filter logic (filtering out)}*)
type filter_type = 
    | Only (* `Only tag will only include elements tagged with tag *)
    | Include (* `Include tag will only include elements not with no-tag. This is useful for languages*)
    | Exclude (* `Exclude tag will only include elements not tagged with tag *)

let filter_type_of_string = function
    | "only" -> Only
    | "include" -> Include
    | "exclude" -> Exclude
    | _ -> failwith "filter type should be one of only, include, exclude"

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
    | true -> 
            let open Tyxml.Html in
            items 
            |> List.map (fun item -> li ~a:[a_class ["yamlcv"]] [item])
            |> ul ~a:[a_class ["yamlcv"]]
            |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
            |> match output_file with 
                | "-" -> CCIO.write_line stdout
                | output_file -> CCIO.(with_out output_file write_line)

let pandoc_filter () =
    let ast_json = Yojson.Basic.from_channel stdin in
    let p = Pandoc.of_json ast_json in
    let filters =
        let get_assoc_list ?(error_message="should be an object") = function
            | `Assoc assoc_list -> assoc_list
            | _ -> failwith error_message
        in
        let ast_json_assoc = get_assoc_list ast_json in
        let meta = List.assoc "meta" ast_json_assoc in
        let meta = get_assoc_list ~error_message:"meta should be an object" meta in
        let filters = List.assoc "filters" meta in
        let filters = get_assoc_list 
            ~error_message:"filters should be an object" filters in
        filters
        |> List.map (function 
            | (filter_type, `String tag) -> 
                    [filter_type_of_string filter_type, tag]
            | (filter_type, `List tags) -> 
                    let filter_type = filter_type_of_string filter_type in
                    tags
                    |>List.map (function 
                        |`String tag -> filter_type, tag
                        | _ -> failwith "filter tags should be a string or a list of strings"
                    ) 
            | _ -> failwith "filter tags should be a string or a list of strings"
        )
        |> List.flatten
    in


(******************************
   {1 Cmdliner stuff}
*)

let cmd = 
    let open Cmdliner in
    let main_term = 
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
        Term.(const main $ wrap_in_ul $ input_arg $ output_arg $ filters)
    in 
    let main_command = 
        let doc = "serialize a yaml file" in
        let info = Cmd.info "yamlcv" ~doc in
        Cmd.v info main_term
    in
    let pandoc_command =
        let doc = "Run as a pandoc filter" in
        let term = Term.(const pandoc_filter $ const ()) in
        let info = Cmd.info "pandoc" ~doc in
        Cmd.v info term
    in
    let info = Cmd.info "yamlcv" ~doc:"CV serialization from yaml files" in
    Cmd.group ~default:main_term info [main_command; pandoc_command]


let () = exit Cmdliner.(Cmd.eval cmd)


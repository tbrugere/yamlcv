exception SerializeError

open Yamlcv
(* open Yamlcv.Base_types *)

let catch_failure f () = 
    try f () 
    with Failure s -> 
        CCIO.write_line stderr "Internal error:";
        CCIO.write_line stderr s; raise (Failure s)

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

let wrap_elements_in_ul elt_list = 
    let open Tyxml.Html in
    elt_list
    |> List.map (fun item -> li ~a:[a_class ["yamlcv"]] [item])
    |> ul ~a:[a_class ["yamlcv"]]

(***************************************************)
(**{1 sorting logic}*)
(*todo maybe several possible sorts in the future? 
i don't know why i would need anything else though*)

(** Sorts by date first, then by "what field" (empty date goes first), 
    then by "where" field
    then just randomly ("precision" field is not an acceptable sorting criterion)
    *)
let sort_elements elements=
    let open Yamlcv.Base_types in
    let compare_dates d1 d2 = 
        let get_first_year (d: date_item) = match d with
            | {date = (Year y); _} -> y
            | {date = (Interval (y, _)); _} -> y
        in
        compare (get_first_year d1) (get_first_year d2)
    in
    let compare_options compare = function
        | None, None -> 0
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some x, Some y -> compare x y
    in
    let compare_elements e1 e2 = 
        match compare_options compare_dates (e1.date, e2.date) with
        | 0 -> begin match compare_options String.compare (e1.what, e2.what) with 
            | 0 -> begin match compare_options String.compare (e1.where, e2.where) with
                | 0 -> Random.int 2 - 1
                | x -> x
            end
            | x -> x
        end
        | x -> x
    in
    let compare_item_or_link i1 i2 = match i1, i2 with
        | `Item i1, `Item i2 -> compare_elements i1 i2
        | _  -> 0
    in 
    let compare_tagged_element (_, e1) (_, e2) = compare_item_or_link e1 e2 in
    let invert f = fun x y -> -(f x y) in
    List.sort (invert compare_tagged_element) elements


(***************************************************)
(**{1 Main code}*)
let main (wrap_in_ul:bool) (input_file:string) (output_file:string) (filters:filter list) () = 
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
        |> sort_elements
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
            items
            |> wrap_elements_in_ul
            |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
            |> match output_file with 
                | "-" -> CCIO.write_line stdout
                | output_file -> CCIO.(with_out output_file write_line)

let pandoc_filter () =
    let ast_json = Yojson.Basic.from_channel stdin in
    let p = Pandoc.of_json ast_json in
    let input_file = Pandoc.meta_string p "yamlcv_file"    in 
    let file_content =  CCIO.(with_in input_file read_all) in
    let yaml = Yaml.of_string_exn file_content in
    let all_items = try serialize yaml 
        with Failure s -> CCIO.write_line stderr s; raise SerializeError
    in 
    let block_map = function 
        | Pandoc.CodeBlock ((_, classes, _), code) 
            when List.mem "yamlcv" classes -> 
                let filters = 
                    code 
                    |> Yaml.of_string_exn 
                    |> (function | `O filters -> filters
                        | _ -> failwith "filters should be an object")
                    |> List.map (function 
                        |(tag, `String filter) -> 
                        (filter_type_of_string filter, tag)
                        | (_, yaml_value) -> failwith [%string "filter should be a string, got %{Yaml.to_string_exn yaml_value}"]
                    )
                in 
                let items = 
                    all_items 
                    |> filter_items filters
                    |> sort_elements
                    |> List.map Html.tagged_item_to_html
                    |> wrap_elements_in_ul
                in
                Some [Pandoc.RawBlock 
                ("html", Format.asprintf "%a" (Tyxml.Html.pp_elt ()) items)
                ]   
        | _ -> None
    in 
    p 
    |> Pandoc.map_blocks block_map (*TODO add inline mapping, for single elem*)
    |> Pandoc.to_json
    |> Yojson.Basic.to_channel stdout

(******************************
   {1 Cmdliner stuff}
*)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let cmd = 
    let open Cmdliner in
    let setup_log =
        Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
    in
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
        Term.(const catch_failure $ (const main $ wrap_in_ul $ input_arg $ output_arg $ filters) $ setup_log)
    in 
    let main_command = 
        let doc = "serialize a yaml file" in
        let info = Cmd.info "yamlcv" ~doc in
        Cmd.v info main_term
    in
    let pandoc_command =
        let doc = "Run as a pandoc filter to html" in
        let term = Term.(const (catch_failure pandoc_filter) $ const ()) in
        let info = Cmd.info "html" ~doc in
        Cmd.v info term
    in
    let info = Cmd.info "yamlcv" ~doc:"CV serialization from yaml files" in
    Cmd.group ~default:main_term info [main_command; pandoc_command]


let () = exit Cmdliner.(Cmd.eval cmd)


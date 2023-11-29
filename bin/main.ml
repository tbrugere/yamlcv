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
let main (wrap_in_ul:bool) (style:Latex.style) (input_file:string) (output_file:string) 
    (filters:filter list) (filetype:[`Html|`Latex|`Unspecified]) () : unit= 
    let file_type: [`Html|`Latex] = match filetype with
        | `Unspecified -> begin match output_file with
            | _  when (String.ends_with ~suffix:"html" output_file)-> `Html
            | _  when (String.ends_with ~suffix:"tex" output_file)-> `Latex
            | "-"| _ -> `Html
        end
        | `Html -> `Html
        | `Latex -> `Latex
    in 
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
    in 
    let output = 
        match file_type with
        | `Html -> Html.tagged_item_list_to_string ~wrap_in_ul items
        | `Latex -> Latex.tagged_item_list_to_string ~style:(style :> [Latex.style | `ComputerWrapped of string]) items
    in 
    match output_file with
        | "-" -> CCIO.write_line stdout output
        | output_file -> CCIO.(with_out output_file write_line output)

let pandoc_filter ~(output_format:[`Html|`Latex]) () =
    let output_format_string = match output_format with
        | `Html -> "html"
        | `Latex -> "latex"
    in
    let ast_json = Yojson.Basic.from_channel stdin in
    let p = Pandoc.of_json ast_json in
    let input_file = Pandoc.meta_string p "yamlcv_file"    in 
    let latex_style =
        try Pandoc.meta_string p "yamlcv_latex_style" |> Latex.style_of_string
        with _ -> `Normal
    in
    let file_content =  CCIO.(with_in input_file read_all) in
    let yaml = Yaml.of_string_exn file_content in
    let all_items = try serialize yaml 
        with Failure s -> CCIO.write_line stderr s; raise SerializeError
    in 
    let block_map = function 
        | Pandoc.CodeBlock ((_, classes, keyvals), code) 
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
                (* let classes_concat = String.concat " ," classes in CCIO.write_line stderr [%string "filters: %{classes_concat}"]; *)
                let latex_style_local = match () with
                    | _ when List.mem "normal" classes -> `Normal
                    | _ when List.mem "language" classes -> `Language
                    | _ when List.mem "computer" classes -> `Computer
                    | _ when List.mem "nocommand" classes -> `NoCommand
                    | _ when List.mem "computerwrap" classes -> `ComputerWrapped (List.assoc "title" keyvals)
                    | _ -> latex_style
                in
                let items = 
                    all_items 
                    |> filter_items filters
                    |> sort_elements
                in
                let output_string = match output_format with
                    | `Html -> Html.tagged_item_list_to_string items
                    | `Latex -> Latex.tagged_item_list_to_string ~style:latex_style_local items
                in 
                Some [Pandoc.RawBlock 
                (output_format_string, 
                output_string)
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

let filetype_of_string = function
    | Some "html" -> `Html
    | Some "latex" -> `Latex
    | None -> `Unspecified
    | _ -> failwith "filetype should be html or latex"

let cmd = 
    let open Cmdliner in
    let setup_log =
        Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
    in
    let main_term: unit Term.t = 
        let input_arg = Arg.(value & opt string "-" & 
        info ["i"; "input"] ~docv:"FILENAME" ~doc:"the yaml input file") in 
        let output_arg = Arg.(value & opt string "-" & 
        info ["o"; "output"] ~docv:"FILENAME" ~doc:"the output file") in
        let wrap_in_ul = Arg.(value & flag  & 
        info ["ul"]  ~doc:"whether to wrap the resulting divs in ul") in
        let latex_style = Arg.(value & opt string "normal" &
        info ["latex-style"] ~docv:"STYLE" ~doc:"the latex style to use") in
        let filetype = Arg.(value & opt (some string) None &
        info ["filetype"] ~docv:"FILETYPE" ~doc:"the output filetype") in
        let filetype: [`Html|`Latex|`Unspecified] Term.t = Term.(const filetype_of_string $ filetype) in
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
        let main_term: (unit -> unit) Term.t =
            Term.(const main 
                $ wrap_in_ul 
                $ (const Latex.style_of_string $ latex_style) 
                $ input_arg 
                $ output_arg 
                $ filters 
                $ filetype
                ) 
        in
        Term.(const catch_failure
            $ main_term
            $ setup_log)
    in 
    let main_command: unit Cmd.t = 
        let doc = "serialize a yaml file" in
        let info = Cmd.info "yamlcv" ~doc in
        Cmd.v info main_term
    in
    let pandoc_command: unit Cmd.t =
        let doc = "Run as a pandoc filter to html" in
        let pandoc_filter = pandoc_filter ~output_format:`Html in
        let term = Term.(const (catch_failure pandoc_filter) $ const ()) in
        let info = Cmd.info "html" ~doc in
        Cmd.v info term
    in
    let pandoc_latex_command: unit Cmd.t =
        let doc = "Run as a pandoc filter to latex" in
        let pandoc_filter = pandoc_filter ~output_format:`Latex in
        let term = Term.(const (catch_failure pandoc_filter) $ const ()) in
        let info = Cmd.info "latex" ~doc in
        Cmd.v info term
    in
    let info = Cmd.info "yamlcv" ~doc:"CV serialization from yaml files" in
    Cmd.group ~default:main_term info [main_command; pandoc_command; pandoc_latex_command]


let () = exit Cmdliner.(Cmd.eval cmd)


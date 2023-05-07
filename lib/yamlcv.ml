(**
   Parsing functions for the yaml CV format
 *)

type single_date = int [@@deriving show]
type date = Year of single_date | Interval of single_date * single_date option [@@deriving show]
type date_item = {
    date: date;
    text: string option;
} [@@deriving make, show]

type link = {
    icon: string option; 
    text: string option; 
    alttext: string option; 
    link: string option; 
} [@@deriving make, show]

type item = {
    date: date_item option;
    what: string option;
    where: string option;
    precision: string option;
} [@@deriving make, show]

type item_cvitem = [`Item of item]
type link_cvitem = [`Link of link]
type cvitem = [item_cvitem | link_cvitem]
(* type cvitem = [`Link of link | `Item of item] *)
(* type tagged_cvitem = Tags.tagset * cvitem *)
(* type tagged_value = Tags.tagsset * string *)

type tag = Tags.tag
type tags = Tags.tagset



let fail error_message yaml = 
    failwith (Printf.sprintf "Error: %s while reading: %s" error_message (Yaml.to_string_exn yaml))

(**
 Reads a list of tags in a yaml list. (from a [tags: [tag1, tag2, ...]] field)
 *)
let read_tag_list = function
    | `A (l) -> l 
        |> List.map ( function |`String s -> s | othervalue -> fail "Tags should be strings" othervalue)
        |> Tags.tagset_of_list
    | othervalue -> fail "Tags should be a list" othervalue

(**
   Reads a yaml object and if it is a dictionary, extracts tags from the eventual [tags: [tag1, tag2, ...]] field.
*)
let get_tags : Yaml.value -> tags * Yaml.value = function
    | (`O association_list) as yaml -> begin match List.assoc_opt "tags" association_list with
        | None -> (Tags.empty, yaml)
        | Some tags -> ((read_tag_list tags), `O (List.remove_assoc "tags" association_list) )
        end
    | yaml -> (Tags.empty, yaml)

(***************************************)
(** {0 Identifying functions.} 
    Check if a yaml object represent a link, an item or a date. 
    (used by the parser)
 *) 

(**check if a yaml object is a cvitem *)
let is_item = function Yaml.(`O association_list) -> 
    association_list 
    |> List.exists (function 
        | "what", _ 
        | "where", _ 
        | "date", _ 
        | "precision", _ -> true 
        | _ -> false) 


(**check if a yaml object is a link*)
let is_link = function Yaml.(`O association_list) -> 
    association_list 
    |> List.exists (function 
        | "icon", _ 
        | "text", _ 
        | "alttext", _ 
        | "link", _ -> true 
        | _ -> false)

(**check if a yaml object is a date*)
let is_date = 
    function 
    |Yaml.(`O association_list) -> association_list 
        |> List.exists (function 
            | "begin", _ 
            | "end", _ -> true 
            | _ -> false)
    | Yaml.(`Float f) -> Float.is_integer f
    | _ -> false

let identify_item =
    function
    | (`O _ | `Float _) as date when is_date date -> `IsDate
    | (`O _) as item when is_item item -> `IsItem
    | (`O _) as link when is_link link -> `IsLink
    | _ -> `IsUnknown

(******************************************************)
(**{0 handling dictionaries of tags}

in the yamlcv format, we can have a dictionary of tagged values, like this:
    tag1: value1
    tag2: value2
    tag3: value3
    ...
where value1, value2, value3 are yaml objects, that evaluate to a list of cv items (either dates / items or links depending on what is allowed in the context)

such a dictionary will be evaluated in the following way
1. each item under a key is tagged with the key (for example the items that value1 evaluates to will be tagged with tag1)
2. each item is tagged with all the other keys in the dictionary in negative form (for example the items under value1 will be tagged with no-tag2 and no-tag3)
3. the resulting list of tagged items is flattened (we could equivalently consider that we use the product scheme, but that would be equivalent )
*) 

(**
when reading a dictionary of tagged values, tag1: value1, tag2: value2, ...
we assume each value is implicitely tagged with all the other tags in negative form.
(so value1 gets the tags tag1, no-tag2, no-tag3, ...)
 *)
let add_implicit_tags: (tag * 'a) list -> (tags * 'a) list = fun tagged_list ->
    let all_tags_negated = 
        tagged_list 
        |> List.map fst 
        |> Tags.tagset_of_list 
        |> Tags.TagMap.map Tags.negate 
    in
    tagged_list
    |> List.map 
        (fun (tag, value) -> 
        (Tags.tag_merge_trump all_tags_negated (Tags.tagset_of_list [tag]) , 
        value))

(**
   when reading an object that represents an item / a date or a link,
   the individual fields may represent lists of tagged items.
   If so we merge them using the following scheme:
       we take the cartesian product of the lists of tagged items
       we eliminate the "impossible cases" tagged with a tag in both positive and negative form (ie with tag and no-tag)

    in the end from a dictionary of 
    key1: value1
    key2: value2
    key3: value3

    where valuei evaluates to a list of tagges items itemsi^1, itemsi^2, itemsi^3, ...
    we obtain a list of tagged items
    where each item is of the form
    key1: items1^u
    key2: items2^v
    key3: items3^w
    where u, v, w are integers

    for all integers where such a combination of u, v, w makes sense 
    (ie the tags of items1^u, items2^v, items3^w are compatible)
*)
let product_merge  (fields_values:  (string * (tags * 'a) list) list): 
    (tags * ((string * 'a) list)) list =
    let fields = List.map fst fields_values in
    let values = List.map snd fields_values in
    let product_accum_list_reverse 
        (l1: (tags * ('a list)) list) 
        (l2: (tags * 'a) list) : (tags * ('a list)) list = 
        let seq1 = List.to_seq l1 in
        let seq2 = List.to_seq l2 in
        Seq.product seq1 seq2
        |> Seq.filter_map 
            begin fun ( (tags1, values1), (tags2, value2) ) -> 
                match Tags.tag_merge tags1 tags2 with
                | None -> None
                | Some tags -> Some (tags, value2::values1)
            end
        |> List.of_seq
    in 
    match values with
    | [] -> failwith "product_merge: empty list"
    | first_value::rest_values -> 
        let init = 
            first_value 
            |> List.map (fun (tags, value) -> (tags, [value]))
        in
        rest_values
        |> List.fold_left product_accum_list_reverse init
        |> List.map (fun (tags, values) -> (tags, List.rev values))
        |> List.map (fun (tags, values) -> (tags, List.combine fields values))


(***************************************)
(** Parsing functions *) 

type _ state = 
    | NoState :  cvitem state
    | ContentState:  [`Text of string] state
    | DateState: [`Date of date_item] state
    | IntState: [`Int of int] state

let rec parse : type a. a state -> tags -> Yaml.value -> (tags * a) list = fun state tags yaml -> 
    let open Yaml in 
    let inner_tags, yaml = get_tags yaml in
    (* inner tags trump outer tags *)
    let tags = Tags.tag_merge_trump tags inner_tags in
    let node_type = identify_item yaml in
    match state, node_type, yaml with
    (*Parsing items*)
    | NoState, _, `String s ->  [tags, `Item (make_item ~what:s ()) ]
    | NoState, `IsItem, `O association_list
        -> (parse_item tags association_list :> (tags * cvitem) list)
    | _, `IsItem, item
        -> fail "Unexpected item while reading item content or date" item

    (* Parsing dates *)
    | DateState, `IsDate, ((`Float _ | `O _) as date ) -> 
            (parse_date tags date :> (tags * a) list)
    | _, `IsDate, date -> fail "Unexpected date" date

    (*parsing links*)
    | NoState, `IsLink, (`O association_list) 
        -> (parse_link tags association_list :> (tags * cvitem) list)

    (* recurse into cases *)
    | _, `IsUnknown, `A (yaml_list) -> yaml_list |>  List.map (parse state tags) |> List.flatten (* list of items *)
    | _, `IsUnknown, `O (tagged_value_list) -> tagged_value_list 
        |> add_implicit_tags
        |> List.map (fun (tags, value) -> parse state (tags) value) 
        |> List.flatten

    (* parsing content *)

    (* unexpected things *)
    | DateState, _, yaml -> fail "Invalid date" yaml
    | _, _,  somethingelse -> fail "Not implemented" somethingelse


    (*     | ContentState  -> [tags, s] *)
    (*     | DateState  -> [tags, s] *)
    (* | _ -> NotSupported *)
and parse_item (tags: tags) (association_list: (string * Yaml.value) list ) : (tags * item_cvitem) list = 
    association_list
    |> List.map (function 
        |( ("what"|"where"|"precision") as field, yaml) 
        -> ((field, (parse ContentState tags yaml)) 
            :> string * (tags * [`Text of string | `Date of date_item]) list)
        | ("date", yaml) 
        -> (("date", (parse DateState tags yaml)
            :> string * (tags * [`Text of string | `Date of date_item]) list)
        )
        | (field, _) -> failwith [%string "Unexpected field in item %{field}"] )
    |> product_merge
    |> List.map (fun (tags, fields_values)->
        let detext = function (`Text s) -> s | (`Date _) -> failwith "impossible" 
        and dedate = function (`Text _) -> failwith "impossible" | (`Date d) -> d 
        in
        let what = List.assoc_opt "what" fields_values |> Option.map detext
        and where = List.assoc_opt "where" fields_values |> Option.map detext
        and precision = List.assoc_opt "precision" fields_values |> Option.map detext
        and date = List.assoc_opt "date" fields_values |> Option.map dedate
        in
        tags, `Item (make_item ?what ?where ?precision ?date ())
    ) 
and parse_link (tags: tags) (association_list: (string * Yaml.value) list ) : (tags * link_cvitem) list =  
    association_list
    |> List.map (fun (field, yaml) -> (field, (parse ContentState tags yaml)))
    |> product_merge
    |> List.map (fun (tags, fields_values)->
        let detext = fun (`Text s) -> s in
        let icon = List.assoc_opt "icon" fields_values |> Option.map detext
        and text = List.assoc_opt "text" fields_values |> Option.map detext
        and alttext = List.assoc_opt "alttext" fields_values |> Option.map detext
        and link = List.assoc_opt "link" fields_values |> Option.map detext
        in
        tags, `Link (make_link ?icon ?text ?alttext ?link ())
    ) 
and parse_date tags date : (tags * [`Date of date_item]) list = match date with 
    |`Float year -> [tags, `Date (make_date_item ~date:(Year (int_of_float year)) ())]
    |(`O fields_values) as date ->
        fields_values (* fields, yaml *)
        |> List.map begin function
            | (("begin" | "end") as field, yaml) -> ((field, parse IntState tags yaml) 
                :> (string * (tags * [`Int of int | `Text of string]) list)  
            )
            | ("text", yaml) -> (("text", parse ContentState tags yaml) 
                :> (string * (tags * [`Int of int | `Text of string]) list)  
            )
            | _ -> fail "Invalid date" date
        end 
        |> product_merge 
        |> List.map (fun (tags, fields_values) -> 
            let text = List.assoc_opt "text" fields_values in
            let text = Option.map (function `Text t->t| _ -> fail "invalid date" date) text in
            let fields_values = List.remove_assoc "text" fields_values in
            let fields_values = 
                List.sort (fun (field1, _) (field2, _) -> compare field1 field2) fields_values in
            let date =  match fields_values with
            | ["begin", `Int b; "end", `Int e; ] -> Interval (b, Some e)
            | ["begin", `Int b] -> Interval (b, None)
            | _ -> fail "Invalid date" date
            in tags, `Date (make_date_item ~date:date ?text:text ())
            )
    (* |(`O ["begin", `Float b; "end", `Float e]  *)
    (* | `O ["end", `Float e; "begin", `Float b])  *)
    (*     -> [tags, Interval (int_of_float b, Some (int_of_float e))] *)
    (* |(`O ["begin", `Float b]) -> [tags, Interval (int_of_float b, None)] *)
    (* | (`O _) as date -> fail "Invalid date" date *)


let parse yaml = 
    parse NoState Tags.empty yaml

open Base_types

type context_item = [`Tag of string | `List of int | `Field of string]
type context = context_item list
let add_to_context ?(field:string option) ?(item_num:int option) ?(tag: string option) (context:context) = 
    match field, item_num, tag with
    | Some field, None, None -> (`Field field)::context
    | None, Some item_num, None -> (`List item_num)::context
    | None, None, Some tag -> (`Tag tag)::context
    | _ -> failwith "add_to_context: only one of field, item_num or tag should be specified"
let context_to_string ?(indentsize:int = 2) (context:context) : string = 
    let rec context_to_string ~(buffer: Buffer.t) 
         ?(indentlevel:int = 0) (context:context) : string = 
        let indentstring = String.make (indentsize * indentlevel) ' ' in
        match context with
        | [] -> Buffer.contents buffer
        | (`List n)::rest -> 
                Buffer.add_string buffer indentstring;
                Buffer.add_string buffer [%string "... (%{n#Int} items) \n"];
                Buffer.add_string buffer indentstring;
                Buffer.add_string buffer "-\n";
                context_to_string ~buffer ~indentlevel:(indentlevel+1) rest
        | (`Field field)::rest -> 
                Buffer.add_string buffer indentstring;
                Buffer.add_string buffer field;
                Buffer.add_string buffer ": (field) \n";
                context_to_string ~buffer ~indentlevel:(indentlevel+1) rest
        | (`Tag tag)::rest -> 
                Buffer.add_string buffer indentstring;
                Buffer.add_string buffer tag;
                Buffer.add_string buffer ": (tag) \n";
                context_to_string ~buffer ~indentlevel:(indentlevel+1) rest
    in 
    let buffer = Buffer.create 100 in
    let rev_context = List.rev context in
    context_to_string ~buffer rev_context

type _ state = 
    | NoState : context -> cvitem state
    | ContentState:  context -> [`Text of string] state
    | DateState: context -> [`Date of date_item] state
    | IntState: context -> [`Int of int] state
let string_of_state : type a. a state -> string = fun state ->
    let build_string state_string context = 
        [%string "%{state_string} in context: \n%{context_to_string context}"] in
    match state with 
    | NoState context -> build_string "NoState" context
    | ContentState context -> build_string "ContentState" context
    | DateState context -> build_string "DateState" context
    | IntState context -> build_string "IntState" context
let add_to_state : type a. ?field:string -> ?item_num:int -> ?tag:string -> a state -> a state = 
    fun ?field ?item_num ?tag state -> 
    let add_to_context = add_to_context ?field ?item_num ?tag in
    match state with
    | NoState context -> NoState (add_to_context  context)
    | ContentState context -> ContentState (add_to_context context)
    | DateState context -> DateState (add_to_context context)
    | IntState context -> IntState (add_to_context  context)

let fail ?state error_message yaml = 
    let state_string = match state with 
    | None -> "" 
    | Some state -> [%string "while in state %{string_of_state state}"] in
    failwith 
    [%string {|Error: %{error_message} %{state_string}
while reading: 
%{Yaml.to_string_exn yaml}|}]

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
let is_item = function `O association_list -> 
    association_list 
    |> List.exists (function 
        | "what", _ 
        | "where", _ 
        | "date", _ 
        | "precision", _ -> true 
        | _ -> false) 


(**check if a yaml object is a link*)
let is_link = function `O association_list -> 
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
    |`O association_list -> association_list 
        |> List.exists (function 
            | "begin", _ 
            | "end", _ -> true 
            | _ -> false)
    | `Float f -> Float.is_integer f
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
let add_implicit_tags: (tag * 'a) list -> (tag * tags * 'a) list = fun tagged_list ->
    let all_tags_negated = 
        tagged_list 
        |> List.map fst 
        |> Tags.tagset_of_list 
        |> Tags.TagMap.map Tags.negate 
    in
    tagged_list
    |> List.map 
        (fun (tag, value) -> 
        (tag, Tags.tag_merge_trump all_tags_negated (Tags.tagset_of_list [tag]) , 
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


(******************************************************)
(**{0 Parsing functions}*)


let rec parse : type a. a state -> tags -> Yaml.value -> (tags * a) list = fun state tags yaml -> 
    let inner_tags, yaml = get_tags yaml in
    (* inner tags trump outer tags *)
    let tags = Tags.tag_merge_trump tags inner_tags in
    let node_type = identify_item yaml in
    match state, node_type, yaml with
    (* basic parsing *)
    | ContentState _, _, `String s -> [tags, `Text s]
    | IntState _, _, `Float f ->  [tags, `Int (int_of_float f)]

    (*Parsing items*)
    | NoState _, _, `String s ->  [tags, `Item (make_item ~what:s ()) ]
    | NoState context, `IsItem, `O association_list
        -> (parse_item context tags association_list :> (tags * cvitem) list)
    | state, `IsItem, item
        -> fail ~state "Unexpected item" item

    (* Parsing dates *)
    | DateState context, `IsDate, ((`Float _ | `O _) as date ) -> 
            (parse_date context tags date :> (tags * a) list)
    | _, `IsDate, date -> fail "Unexpected date" date

    (*parsing links*)
    | NoState context, `IsLink, (`O association_list) 
        -> (parse_link context tags association_list :> (tags * cvitem) list)

    (* recurse into cases *)
    | state, `IsUnknown, `A (yaml_list) -> 
        yaml_list 
        |> List.mapi 
            (fun item_num yaml -> parse (add_to_state ~item_num state) tags yaml) 
        |> List.flatten (* list of items *)
    | state, `IsUnknown, `O (tagged_value_list) -> tagged_value_list 
        |> add_implicit_tags
        |> List.map 
            (fun (tag, tags, value) -> parse (add_to_state ~tag state) tags value) 
        |> List.flatten

    (* unexpected things *)
    | (DateState _) as state, _, yaml -> fail ~state "Invalid date" yaml
    | state, _,  somethingelse -> fail ~state "Serializing error" somethingelse


    (*     | ContentState  -> [tags, s] *)
    (*     | DateState  -> [tags, s] *)
    (* | _ -> NotSupported *)
and parse_item (context:context) (tags: tags) (association_list: (string * Yaml.value) list ) : (tags * item_cvitem) list = 
    association_list
    |> List.map (function 
        |( ("what"|"where"|"precision") as field, yaml) 
        -> ((field, (parse (ContentState (add_to_context ~field context)) tags yaml)) 
            :> string * (tags * [`Text of string | `Date of date_item]) list)
        | ("date" as field, yaml) 
        -> (("date", (parse (DateState (add_to_context ~field context)) tags yaml)
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
and parse_link (context:context) (tags: tags) (association_list: (string * Yaml.value) list ) : (tags * link_cvitem) list =  
    association_list
    |> List.map (fun (field, yaml) -> (field, (parse (ContentState (add_to_context ~field context)) tags yaml)))
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
and parse_date (context:context) (tags:tags) date : (tags * [`Date of date_item]) list = 
    match date with 
    |`Float year -> [tags, `Date (make_date_item ~date:(Year (int_of_float year)) ())]
    |(`O fields_values) as date ->
        fields_values (* fields, yaml *)
        |> List.map begin function
            | (("begin" | "end") as field, yaml) -> 
                ((field, 
                parse (IntState (add_to_context ~field context)) tags yaml) 
                :> (string * (tags * [`Int of int | `Text of string]) list)  
            )
            | ("text" as field, yaml) -> 
                (("text", 
                parse (ContentState (add_to_context ~field context)) tags yaml) 
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

let parse yaml = 
    parse (NoState []) Tags.empty yaml

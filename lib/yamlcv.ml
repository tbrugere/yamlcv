
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
    date: date option;
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

(* let is_item_O: *)


type _ state = 
    | NoState :  cvitem state
    | ContentState:  string state
    | DateState: date state
    | IntState: int state


(* let merge_tagged_lists =  *)
    (* How two tagged lists are merged *)

let fail error_message yaml = 
    failwith (Printf.sprintf "Error: %s while reading: %s" error_message (Yaml.to_string_exn yaml))

let read_tag_list = function
    | `A (l) -> List.map ( function |`String s -> s | othervalue -> fail "Tags should be strings" othervalue) l
    | othervalue -> fail "Tags should be a list" othervalue

let get_tags : Yaml.value -> tags * Yaml.value = function
    | (`O association_list) as yaml -> begin match List.assoc_opt "tags" association_list with
        | None -> ([], yaml)
        | Some tags -> ((read_tag_list tags), `O (List.remove_assoc "tags" association_list) )
        end
    | yaml -> ([], yaml)

let is_item = function Yaml.(`O association_list) -> 
    association_list 
    |> List.exists (function 
        | "what", _ 
        | "where", _ 
        | "date", _ 
        | "precision", _ -> true 
        | _ -> false) 

let is_link = function Yaml.(`O association_list) -> 
    association_list 
    |> List.exists (function 
        | "icon", _ 
        | "text", _ 
        | "alttext", _ 
        | "link", _ -> true 
        | _ -> false)

let is_date = 
    function 
    |Yaml.(`O association_list) -> association_list 
        |> List.exists (function 
            | "begin", _ 
            | "end", _ -> true 
            | _ -> false)
    | Yaml.(`Float f) -> Float.is_integer f
    | _ -> false

let rec parse : type a. a state -> tag list -> Yaml.value -> (tags * a) list = fun state tags yaml -> 
    let open Yaml in 
    let inner_tags, yaml = get_tags yaml in
    let tags = tags @ inner_tags in
    match state, yaml with

    (*Parsing items*)
    | NoState, `String s ->  [tags, `Item (make_item ~what:s ()) ]
    | NoState, (`O association_list as item) when is_item item 
        -> (parse_item association_list :> (tags * cvitem) list)
    | _, ((`O _) as item) when is_item item 
        -> fail "Unexpected item while reading item or date" item

    (* Parsing dates *)
    | DateState, ((`Float _ | `O _) as date ) when is_date date -> parse_date tags date
    (*parsing links*)
    | NoState, (`O association_list as link) when is_link link 
        -> (parse_link association_list :> (tags * cvitem) list)

    | _, date when is_date date -> fail "Unexpected date" date
    | _, `A (yaml_list) -> yaml_list |>  List.map (parse state tags) |> List.flatten
    | DateState, yaml -> fail "Invalid date" yaml
    | _, somethingelse -> fail "Not implemented" somethingelse
    (*     | ContentState  -> [tags, s] *)
    (*     | DateState  -> [tags, s] *)
    (* | _ -> NotSupported *)
and parse_item (association_list: (string * Yaml.value) list ) : (tags * item_cvitem) list = failwith "NI"
and parse_link (association_list: (string * Yaml.value) list ) : (tags * link_cvitem) list = failwith "NI"
and parse_date tags date : (tags * date) list   = match date with 
    |`Float year -> [tags, Year (int_of_float year)]
    |(`O ["begin", b; "end", e] | `O ["end", e; "begin", b]) 
        -> [tags, Interval (int_of_float b, Some (int_of_float e))]
    |(`O ["begin", b]) -> [tags, Interval (int_of_float b, None)]
    | `O date -> fail "Invalid date" date


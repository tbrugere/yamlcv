type tag = string

type tag_type = [`Positive | `Negative]
let negate = function
    | `Positive -> `Negative
    | `Negative -> `Positive

module TagMap = Map.Make(String)
type tagset = tag_type TagMap.t
let empty = TagMap.empty

let tagset_to_stringlist tagset = 
    let tagset = TagMap.bindings tagset in
    let tagset = List.map (fun (tag, value) -> 
        match value with
        | `Positive -> tag
        | `Negative -> "no-" ^ tag
    ) tagset in
    tagset
let tagset_to_string tagset = 
    String.concat "," (tagset_to_stringlist tagset)

let pp_tagset : tagset Fmt.t = Fmt.of_to_string tagset_to_string

(** 
Merge two tagsets, where the second tagset takes precedence over the first
[tagm_merge_trump a b] merges a and b, 
if there is a conflict, the value from b is taken
*)
let tag_merge_trump: tagset -> tagset -> tagset = TagMap.merge (fun _ a b -> match a, b with
  | _, Some a | Some a, None -> Some a
  | None, None -> None
)


exception InAndOut

(** Merge two tagsets
[tagm_merge_trump a b] merges a and b, 
- if there is a conflict (ie one tag is [`Positive] in one of the tagsets, 
and [`Negative] in the other), returns None
otherwise 
@tags1 the first tagset
@tags2 the second tagset
@returns [Some merged_tagset], or [None] if there was a conflict
*)

let tag_merge tags1 tags2 = 
    try 
        Some (TagMap.merge (fun _ a b -> match a, b with
            | Some `Negative, Some `Positive
            | Some `Positive, Some `Negative -> raise InAndOut
            | _, Some a | Some a, None -> Some a
            | None, None -> None
        ) tags1 tags2)
    with 
    | InAndOut -> None


(**
   Converts a string to a tag and a tag_type
*)
let rec tag_of_string s = match () with
    | _ when String.starts_with  ~prefix:"no-" s ->
            let s = String.sub s 3 (String.length s - 3) in
            let value, tagname = tag_of_string s in
            (negate value, tagname)
    | _ -> (`Positive, s)

let rec tagset_of_list = function
    | [] -> TagMap.empty
    | x :: xs -> 
        let value, tagname = tag_of_string x in
        TagMap.add tagname value (tagset_of_list xs)



type tag = string

type tag_type = [`Positive | `Negative]
let negate = function
    | `Positive -> `Negative
    | `Negative -> `Positive

module TagMap = Map.Make(String)
type tagset = tag_type TagMap.t

let tag_merge_trump = TagMap.merge (fun _ a b -> match a, b with
  | _, Some a | Some a, None -> Some a
  | None, None -> None
)

exception InAndOut
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


let rec of_string s =

    


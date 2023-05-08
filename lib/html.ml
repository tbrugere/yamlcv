(**{0 HTML serialization}*)

open Base_types
open Tyxml

let year_to_html ?(present_string="Present") (year: int option) = 
    let open Tyxml.Html in
    match year with
    | Some year -> let year = string_of_int year in
            time ~a:[a_class ["yamlcv-year"]; a_datetime year] [txt year]
    | None -> span ~a:[a_class ["yamlcv-present"]] [txt present_string]

let date_to_html ?(present_string="Present") date = 
    let open Tyxml.Html in
    match date with
    | Year year -> [year_to_html ~present_string (Some year)]
    | Interval (year1, year2) -> 
            [year_to_html ~present_string (Some year1); 
            txt "—"; 
            year_to_html ~present_string year2]

let date_item_to_html ?(present_string="Present") date_item = 
    let open Tyxml.Html in
    match date_item with
    | {date=_; text=Some text } ->[txt text] 
    | {date ; text=None} -> date_to_html ~present_string date

let item_to_html ?(present_string="Present") ?(tags=[]) item = 
    let open Tyxml.Html in
    let date_html = match item.date with
        | Some date ->  
            span ~a:[a_class ["yamlcv-date"]] (date_item_to_html ~present_string date)
        | None -> txt ""
    and what_html = match item.what with
        | Some what -> span ~a:[a_class ["yamlcv-what"]] [txt what]
        | None -> txt ""
    and where_html = match item.where with
        | Some where -> span ~a:[a_class ["yamlcv-where"]] [txt where]
        | None -> txt ""
    and precision_html = match item.precision with
        | Some precision -> span ~a:[a_class ["yamlcv-precision"]] [txt precision]
        | None -> txt ""
    in
    div ~a:[a_class ("yamlcv-item"::tags)] [date_html; what_html; where_html; precision_html]

(*maybe later add support for inlining svg icons*)
let icon_to_html icon alttext = 
    let open Tyxml.Html in
    img ~src:icon ~a:[a_class ["yamlcv-icon"]] ~alt:alttext ()

let link_to_html ?(tags=[]) link_ = 
    let open Tyxml.Html in
    let alttext = Option.value link_.alttext ~default:"" in
    let icon_html = match link_.icon with
        | Some icon -> [icon_to_html icon alttext]
        | None -> []
    and text_html = match link_.text with
        | Some text -> [txt text]
        | None -> []
    and link_destination = match link_.link with
        | Some destination -> destination
        | None -> ""
    in
    a ~a:[a_href link_destination; a_class ("yamlcv-link"::tags)] (text_html @ icon_html)

let tagged_item_to_html (tags, cvitem) = 
    let tags_style =
        tags
        |> Tags.tagset_to_stringlist
        |> List.map (String.cat "yamlcv-tag-")
        |> List.cons "yamlcv" (*add the yamlcv tag*)
    in
    let open Html in
    match cvitem with
    | `Item item -> item_to_html ~tags:tags_style item
    | `Link link -> link_to_html ~tags:tags_style link



(**{0 Latex serialization}*)

open Base_types


type latex= 
    [ `String of string
    | `Concat of latex list
    | `Command of string * latex list
    | `CommandOptional of string * latex list * latex list
    | `Environment of string * latex list
    | `Comment of string
    ]

let empty_latex = `String ""

(* type t = latex *)

let rec latex_to_string = function
    | `String s -> s
    | `Concat l -> List.map latex_to_string l |> String.concat ""
    | `Command (name, args) -> 
            let args = List.map latex_to_string args |> String.concat "}{" in
            Printf.sprintf "\\%s{%s}" name args
    | `CommandOptional (name, args, optional_args) -> 
            let args = List.map latex_to_string args |> String.concat "}{" in
            let optional_args = List.map latex_to_string optional_args |> String.concat "}{" in
            Printf.sprintf "\\%s[%s]{%s}" name optional_args args
    | `Environment (name, args) -> 
            let args = List.map latex_to_string args |> String.concat "" in
            Printf.sprintf "\\begin{%s}%s\\end{%s}" name args name
    | `Comment s -> Printf.sprintf "%% %s" s

type cventry_type = {
    date: latex;
    what: latex;
    where: latex;
    localization: latex;
    grade: latex;
    comment: latex;
}

type cvlanguage_type = {
    language: latex;
    level: latex;
    comment: latex;
}

type cvcomputer_type = {
    category: latex;
    programs: latex;
}

type style = [`Normal | `Language | `Computer | `NoCommand]
let style_of_string = function
    | "normal" -> `Normal
    | "language" -> `Language
    | "computer" -> `Computer
    | "nocmd" -> `NoCommand
    | _ -> failwith "unknown style, should be one of normal, language, computer or nocmd"

type moderncv = 
    [ `CVentry of cventry_type 
    | `CVlanguage of cvlanguage_type
    | `CVcomputer of cvcomputer_type
    | `WebLink of latex * string
    | `HttpLink of latex * string
    | `EmailLink of latex * string
    | `NoCommand of latex
    ]



let year_to_latex ?(present_string="Present") (year: int option) : latex = 
    match year with
    | Some year -> let year = string_of_int year in
        `String year
    | None -> 
        `String present_string

let date_to_latex ?(present_string="Present") date : latex = 
    match date with
    | Year year -> year_to_latex ~present_string (Some year)
    | Interval (year1, year2) -> 
            `Concat
            [year_to_latex ~present_string (Some year1); 
            `String "--"; 
            year_to_latex ~present_string year2]

let date_item_to_latex ?(present_string="Present") date_item = 
    match date_item with
    | {date=_; text=Some text } -> `String text
    | {date ; text=None} -> date_to_latex ~present_string date

let item_to_latex ?(style=`Normal) ?(present_string="Present") (item: Base_types.item) = 
    let date_latex = match item.date with
        | Some date -> date_item_to_latex ~present_string date
        | None -> `String ""
    and what_latex = match item.what with
        | Some what -> `String what
        | None -> `String ""
    and where_latex = match item.where with
        | Some where -> `String where
        | None -> `String ""
    and precision_latex = match item.precision with
        | Some precision -> `String precision
        | None -> `String ""
    in match style with
    | `Normal -> `CVentry {date=date_latex; what=what_latex; where=where_latex; localization=empty_latex; grade=empty_latex; comment=precision_latex}
    | `Language -> `CVlanguage {language=what_latex; level=where_latex; comment=precision_latex}
    | `Computer -> `CVcomputer {category=what_latex; programs=precision_latex}
    | `NoCommand -> `NoCommand (`Concat [date_latex; `String " "; what_latex; `String " "; where_latex; `String " "; precision_latex])


(* (*maybe later add support for inlining svg icons*) *)
(* let icon_to_html icon alttext =  *)
(*     let open Tyxml.Html in *)
(*     img ~src:icon ~a:[a_class ["yamlcv-icon"]] ~alt:alttext () *)

let decompose_link link = 
     let open String in
    let len_http = String.length "http://" 
    and len_mailto = String.length "mailto:" in
    (match () with
    | _ when (starts_with ~prefix:"http://" link ) -> 
            let link = sub link len_http ((length link) - len_http)  in
            `Http link
    | _ when (starts_with ~prefix:"mailto:" link ) ->
            let link = sub link len_mailto ((length link) - len_mailto)  in
            `Mailto link
    | _ -> (`Web link)
    )

let link_to_latex  (link_: link) = 
    (* let alttext = Option.value link_.alttext ~default:"" in *)
    (*icons not supported for now*)
    (* let icon_html = match link_.icon with *)
    (*     | Some icon -> [icon_to_html icon alttext] *)
    (*     | None -> [] *)
    let text_latex = match link_.text with
        | Some text -> `String text
        | None -> begin match link_.link with 
            | Some destination -> `String destination
            | None -> `String ""
            end
    and link_destination = match link_.link with
        | Some destination -> destination
        | None -> ""
    in
    match decompose_link link_destination with
    | `Http link -> `HttpLink (text_latex, link)
    | `Mailto link -> `EmailLink (text_latex, link)
    | `Web link -> `WebLink (text_latex, link)

        

let tagged_item_to_cvitem ?(style=`Normal) (tags, cvitem) = 
    (* let tags_style = *)
    (*     tags *)
    (*     |> Tags.tagset_to_stringlist *)
    (*     |> List.map (String.cat "yamlcv-tag-") *)
    (*     |> List.cons "yamlcv" (*add the yamlcv tag*) *)
    (* in *)
    ignore tags;
    match cvitem with
    | `Item item -> item_to_latex ~style item
    | `Link link -> link_to_latex link


let cvitem_to_latex  cvitem = 
    match cvitem with
    | `CVentry entry -> 
            let arguments = [entry.date; entry.what; entry.where; entry.localization; entry.grade; entry.comment] in
            `Command ("cventry", arguments)
    | `CVlanguage language -> `Command ("cvlanguage", [language.language; language.level; language.comment])
    | `CVcomputer computer -> `Command ("cvcomputer", [computer.category; computer.programs])
    | `WebLink (text, link) -> `CommandOptional ("weblink", [text], [`String link])
    | `HttpLink (text, link) -> `CommandOptional ("httplink", [text], [`String link])
    | `EmailLink (text, link) -> `CommandOptional ("emaillink", [text], [`String link])
    | `NoCommand latex -> latex

let tagged_item_list_to_string ?(style:[style|`ComputerWrapped of string]=`Normal)  items =
    let ti_to_cvitem, (eventually_wrap_in_computer: moderncv list-> moderncv list)= match style with
    | #style as style -> (tagged_item_to_cvitem ~style, (fun x -> x))
    | `ComputerWrapped title -> 
            (tagged_item_to_cvitem ~style:`NoCommand, 
            (fun x -> [`CVcomputer {category=(`String title); programs=(`String (x |> List.map cvitem_to_latex |> List.map latex_to_string |> String.concat ", "))}]))
    in
    items
    |> List.map ti_to_cvitem
    |> eventually_wrap_in_computer
    |> List.map cvitem_to_latex
    |> List.map latex_to_string
    |> String.concat "\n"


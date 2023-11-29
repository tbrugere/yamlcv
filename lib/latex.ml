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


type moderncv_info = { firstname: string option;
    lastname: string option;
    address: string option;
    phone: string option;
    email: string option;
    social: (string * string * string) list;
    photo: string option;
    quote: string option;
    website: (string*string) option;
}

let empty_moderncv_info = {
    firstname=None;
    lastname=None;
    address=None;
    phone=None;
    email=None;
    social=[];
    photo=None;
    quote=None;
    website=None;
}

let get_latex_info ?(include_photo=false) (items: (Base_types.tags * Base_types.cvitem) list ) =
    let info = List.fold_left (
        let open Base_types in
        fun info (tags, item) -> match item with
        | `Item {what; _} when Tags.tagset_contains tags "firstname"  
            -> {info with firstname=what}
        | `Item {what; _} when Tags.tagset_contains tags "lastname" 
            -> {info with lastname=what}
        | `Item {what=Some what; _} when Tags.tagset_contains tags "name" 
            -> 
                let firstname, lastname = 
                    match String.split_on_char ' ' what with
                    | [firstname; lastname] -> firstname, lastname
                    | _ -> what, ""
                in
                {info with firstname=Some firstname; lastname=Some lastname}
        | `Item {what; _} when Tags.tagset_contains tags "address" 
            -> {info with address=what}
        | `Item {what; _} when Tags.tagset_contains tags "tel"  
                                || Tags.tagset_contains tags "phone" 
            -> {info with phone=what}
        | `Item {what; _} when Tags.tagset_contains tags "email" 
            -> {info with email=what}
        | `Item {what; _} when Tags.tagset_contains tags "photo" 
            -> {info with photo=what}
        | `Item {what; _} when Tags.tagset_contains tags "quote" 
            -> {info with quote=what}
        | `Link {link; text; alttext; _} 
            when Tags.tagset_contains tags "social" 
            -> let link = Option.value link ~default:"" in
                {info with social=(link, 
            Option.value text ~default:link, 
            Option.value ~default:"" alttext)::info.social}
        | `Link {link; text;  _} when Tags.tagset_contains tags "website" 
            -> let link = Option.value link ~default:"" in
                let text = Option.value text ~default:link in
                {info with website=Some (link, text)}
        | _ -> info
    ) empty_moderncv_info items
    in
    let latex_list = [] in 
    let latex_list = match info.firstname with None -> latex_list
    | Some firstname -> (`Command ("firstname", [`String firstname]))::latex_list
    in let latex_list = match info.lastname with None -> latex_list
    | Some lastname -> (`Command ("familyname", [`String lastname]))::latex_list
    in let latex_list = match info.address with None -> latex_list
    | Some address -> (`Command ("address", [`String address]))::latex_list
    in let latex_list = match info.phone with None -> latex_list
    | Some phone -> (`Command ("phone", [`String phone]))::latex_list
    in let latex_list = match info.email with None -> latex_list
    | Some email -> (`Command ("email", [`String email]))::latex_list
    in
    let latex_list = match info.photo with
        | Some photo when include_photo 
            -> (`Command ("photo", [`String photo]))::latex_list
        | _ -> latex_list
    in
    let latex_list = match info.quote with None -> latex_list
        | Some quote -> (`Command ("quote", [`String quote]))::latex_list
    in
    let latex_list = info.social
        |> List.map (
            fun (_, text, alt) -> `CommandOptional ("social", [`String alt], [`String text]))
        |> List.rev_append latex_list
    in 
    let latex_list = match info.website with None -> latex_list
        | Some (link, text) -> (`Command ("extrainfo", [ `CommandOptional ("weblink", [`String text], [`String link] ) ]
        )::latex_list)
    in 
    `Concat latex_list
        |> latex_to_string


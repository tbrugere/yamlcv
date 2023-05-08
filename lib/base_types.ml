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

type item_cvitem = [`Item of item][@@deriving show]
type link_cvitem = [`Link of link][@@deriving show]
type cvitem = [item_cvitem | link_cvitem] [@@deriving show]

type tag = Tags.tag
type tags = Tags.tagset

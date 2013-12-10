open Omd_utils
open Printf

(** references, instances created in [Omd_parser.main_parse] and
    accessed in the [Omd_backend] module. *)
module R = Map.Make(String)
class ref_container : object
    val mutable c : (string * string) R.t
    method add_ref : R.key -> string -> string -> unit
    method get_ref : R.key -> (string * string) option
    method get_all : (string * (string * string)) list
  end = object
  val mutable c = R.empty
  val mutable c2 = R.empty

  method get_all = R.bindings c

  method add_ref name title url =
    c <- R.add name (url, title) c;
    let ln = String.lowercase (String.copy name) in
    if ln <> name then c2 <- R.add ln (url, title) c2

  method get_ref name =
    try
      let (url, title) as r =
        try R.find name c
        with Not_found ->
          let ln = String.lowercase (String.copy name) in
          try R.find ln c
          with Not_found ->
            R.find ln c2
      in Some r
    with Not_found ->
      None
end

type element =
  | H1 of t
  | H2 of t
  | H3 of t
  | H4 of t
  | H5 of t
  | H6 of t
  | Paragraph of t
  | Text of string
  | Emph of t
  | Bold of t
  | Ul of t list
  | Ol of t list
  | Ulp of t list
  | Olp of t list
  | Code of name * string
  | Code_block of name * string
  | Br
  | Hr
  | NL
  | Url of href * t * title
  | Ref of ref_container * name * string * fallback
  | Img_ref of ref_container * name * alt * fallback
  | Html of string
  | Html_block of string
  | Html_comment of string
  | Blockquote of t
  | Img of alt * src * title
  | X of
      < name : string;
        to_html : ?indent:int -> (t -> string) -> t -> string option;
        to_sexpr : (t -> string) -> t -> string option;
        to_t : t -> t option >
and fallback = string
and name = string
and alt = string
and src = string
and href = string
and title = string
and t = element list

let rec loose_compare t1 t2 = match t1,t2 with
  | H1 e1::tl1, H1 e2::tl2
  | H2 e1::tl1, H2 e2::tl2
  | H3 e1::tl1, H3 e2::tl2
  | H4 e1::tl1, H4 e2::tl2
  | H5 e1::tl1, H5 e2::tl2
  | H6 e1::tl1, H6 e2::tl2
  | Emph e1::tl1, Emph e2::tl2
  | Bold e1::tl1, Bold e2::tl2
  | Blockquote e1::tl1, Blockquote e2::tl2
  | Paragraph e1::tl1, Paragraph e2::tl2
      ->
      (match loose_compare e1 e2 with
         | 0 -> loose_compare tl1 tl2
         | i -> i)

  | Ul e1::tl1, Ul e2::tl2
  | Ol e1::tl1, Ol e2::tl2
  | Ulp e1::tl1, Ulp e2::tl2
  | Olp e1::tl1, Olp e2::tl2
      ->
      (match loose_compare_lists e1 e2 with
         | 0 -> loose_compare tl1 tl2
         | i -> i)

  | (Code _ as e1)::tl1, (Code _ as e2)::tl2
  | (Br as e1)::tl1, (Br as e2)::tl2
  | (Hr as e1)::tl1, (Hr as e2)::tl2
  | (NL as e1)::tl1, (NL as e2)::tl2
  | (Html _ as e1)::tl1, (Html _ as e2)::tl2
  | (Html_block _ as e1)::tl1, (Html_block _ as e2)::tl2
  | (Html_comment _ as e1)::tl1, (Html_comment _ as e2)::tl2
  | (Img _ as e1)::tl1, (Img _ as e2)::tl2
  | (Text _ as e1)::tl1, (Text _ as e2)::tl2
      ->
      (match compare e1 e2 with
         | 0 -> loose_compare tl1 tl2
         | i -> i)

  | Code_block(l1,c1)::tl1, Code_block(l2,c2)::tl2
      ->
      (match compare l1 l2, String.length c1 - String.length c2 with
         | 0, 0 ->
             (match compare c1 c2 with
               | 0 -> loose_compare tl1 tl2
               | i -> i)
         | 0, 1 ->
             (match compare c1 (c2^"\n") with
                | 0 -> loose_compare tl1 tl2
                | i -> i)
         | 0, -1 ->
             (match compare (c1^"\n") c2 with
                | 0 -> loose_compare tl1 tl2
                | i -> i)
         | i, _ -> i
      )

  | Url (href1, t1, title1)::tl1, Url (href2, t2, title2)::tl2
      ->
      (match compare href1 href2 with
         | 0 -> (match loose_compare t1 t2 with
                   | 0 -> (match compare title1 title2 with
                             | 0 -> loose_compare tl1 tl2
                             | i -> i)
                   | i -> i)
         | i -> i)

  | Ref (ref_container1, name1, x1, fallback1)::tl1,
        Ref (ref_container2, name2, x2, fallback2)::tl2
  | Img_ref (ref_container1, name1, x1, fallback1)::tl1,
        Img_ref (ref_container2, name2, x2, fallback2)::tl2
        ->
      (match compare (name1, x1) (name2, x2) with
         | 0 ->
             let cff =
               match compare fallback1 fallback2 with
                | 0 -> 0
                | i ->
                    match String.length fallback2 - String.length fallback1 with
                      | 2 -> compare (fallback1 ^ "[]") fallback2
                      | -2 -> compare fallback1 (fallback2 ^ "[]")
                      | _ -> i
             in
               if cff = 0 then
                 match
                   compare (ref_container1#get_all) (ref_container2#get_all)
                 with
                   | 0 -> loose_compare tl1 tl2
                   | i -> i
               else
                 cff
         | i -> i)

  | X e1::tl1, X e2::tl2 ->
      (match compare (e1#name) (e2#name) with
         | 0 -> (match compare (e1#to_t) (e2#to_t) with
                   | 0 -> loose_compare tl1 tl2
                   | i -> i)
         | i -> i)
  | X _::_, _ -> 1
  | _, X _::_ -> -1
  | _ -> compare t1 t2

and loose_compare_lists l1 l2 =
  match l1, l2 with
    | [], [] -> 0
    | e1::tl1, e2::tl2 ->
        (match loose_compare e1 e2 with
           | 0 -> loose_compare_lists tl1 tl2
           | i -> i)
    | _, [] -> 1
    | _ -> -1

type tok = (* Cs(n) means (n+2) times C *)
| Ampersand
| Ampersands of int
| At
| Ats of int
| Backquote
| Backquotes of int
| Backslash
| Backslashs of int
| Bar
| Bars of int
| Caret
| Carets of int
| Cbrace
| Cbraces of int
| Colon
| Colons of int
| Comma
| Commas of int
| Cparenthesis
| Cparenthesiss of int
| Cbracket
| Cbrackets of int
| Dollar
| Dollars of int
| Dot
| Dots of int
| Doublequote
| Doublequotes of int
| Exclamation
| Exclamations of int
| Equal
| Equals of int
| Greaterthan
| Greaterthans of int
| Hash
| Hashs of int
| Lessthan
| Lessthans of int
| Minus
| Minuss of int
| Newline
| Newlines of int
| Number of string
| Obrace
| Obraces of int
| Oparenthesis
| Oparenthesiss of int
| Obracket
| Obrackets of int
| Percent
| Percents of int
| Plus
| Pluss of int
| Question
| Questions of int
| Quote
| Quotes of int
| Semicolon
| Semicolons of int
| Slash
| Slashs of int
| Space
| Spaces of int
| Star
| Stars of int
| Tab
| Tabs of int
| Tilde
| Tildes of int
| Underscore
| Underscores of int
| Word of string
| Tag of extension

and extension = (t -> tok list -> tok list -> ((t * tok list * tok list) option))

type extensions = extension list

let rec normalise_md l =
  let rec loop = function
    | [NL;NL;NL;NL;NL;NL;NL;]
    | [NL;NL;NL;NL;NL;NL;]
    | [NL;NL;NL;NL;NL;]
    | [NL;NL;NL;NL;]
    | [NL;NL;NL;]
    | [NL;NL]
    | [NL] -> []
    | [] -> []
    | NL::NL::NL::tl -> loop (NL::NL::tl)
    | Text t1::Text t2::tl -> loop (Text(t1^t2)::tl)
    | NL::(((Paragraph _|Html_block _|H1 _|H2 _|H3 _|H4 _|H5 _|H6 _|Code_block _|Ol _|Ul _|Olp _|Ulp _)::_) as tl) -> loop tl
    | Paragraph[Text " "]::tl -> loop tl
    | Paragraph[]::tl -> loop tl
    | Paragraph(p)::tl -> Paragraph(loop p)::loop tl
    | H1 v::tl -> H1(loop v)::loop tl
    | H2 v::tl -> H2(loop v)::loop tl
    | H3 v::tl -> H3(loop v)::loop tl
    | H4 v::tl -> H4(loop v)::loop tl
    | H5 v::tl -> H5(loop v)::loop tl
    | H6 v::tl -> H6(loop v)::loop tl
    | Emph v::tl -> Emph(loop v)::loop tl
    | Bold v::tl -> Bold(loop v)::loop tl
    | Ul v::tl -> Ul(List.map loop v)::loop tl
    | Ol v::tl -> Ol(List.map loop v)::loop tl
    | Ulp v::tl -> Ulp(List.map loop v)::loop tl
    | Olp v::tl -> Olp(List.map loop v)::loop tl
    | Blockquote v::tl -> Blockquote(loop v)::loop tl
    | Url(href,v,title)::tl -> Url(href,(loop v),title)::loop tl
    | Text _
    | Code _
    | Code_block _
    | Br
    | Hr
    | NL
    | Ref _
    | Img_ref _
    | Html _
    | Html_block _
    | Html_comment _
    | Img _
    | X _ as v::tl -> v::loop tl
  in
  let a = loop l in
  let b = loop a in
  if a = b then
    a
  else
    normalise_md b

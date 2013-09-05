open Omd_utils
open Printf

(** references, instances created in [Omd_parser.main_parse] and
    accessed in the [Omd_backend] module. *)
module R = Map.Make(String)
class ref_container = object
  val broken_url = "", "Broken URL"
  val mutable c = R.empty
  val mutable c2 = R.empty

  method get_all = R.bindings c

  method add_ref name title url =
    c <- R.add name (url, title) c;
    let ln = String.lowercase (String.copy name) in
    if ln <> name then c2 <- R.add ln (url, title) c2

  method get_ref name =
    let (url, title) as r =
      try R.find name c
      with Not_found ->
        let ln = String.lowercase (String.copy name) in
        try R.find ln c
        with Not_found ->
          try R.find ln c2
          with Not_found ->
            if debug then eprintf "Could not find reference (%s)\n%!" (name);
            broken_url
    in r
end

type element =
| Paragraph of t
| Text of string
| Emph of t
| Bold of t
| Ul of t list
| Ol of t list
| Ulp of t list
| Olp of t list
| Code of string (* html entities are to be converted *later* *)
| Code_block of string (* html entities are to be converted *later* *)
| Br
| Hr
| Url of href * t * title
| Ref of ref_container * name * string
| Img_ref of ref_container * name * alt
| Html of string
| Html_block of string
| Html_comments of string
| H1 of t
| H2 of t
| H3 of t
| H4 of t
| H5 of t
| H6 of t
| Blockquote of t
| Img of alt * src * title
| NL
| X of (<
        name: string;
        to_html: ?indent:int -> (t -> string) -> t -> string option;
        to_sexpr: (t -> string) -> t -> string option;
        to_t: t -> t option >)
and name = string
and alt = string
and src = string
and href = string
and title = string
and t = element list


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


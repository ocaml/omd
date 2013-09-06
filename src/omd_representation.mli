module R :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
class ref_container :
  object
    val broken_url : string * string
    val mutable c : (string * string) R.t
    method add_ref : R.key -> string -> string -> unit
    method get_ref : R.key -> string * string
    method get_all : (string * (string * string)) list
  end
type element =
    Paragraph of t
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
  | X of
      < name : string;
        to_html : ?indent:int -> (t -> string) -> t -> string option;
        to_sexpr : (t -> string) -> t -> string option;
        to_t : t -> t option >
and name = string
and alt = string
and src = string
and href = string
and title = string
and t = element list
type tok =
    Ampersand
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
and extension = t -> tok list -> tok list -> (t * tok list * tok list) option
type extensions = extension list

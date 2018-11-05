
module R : Map.S with type key = string

class ref_container :
  object
    val mutable c : (string * string) R.t
    method add_ref : R.key -> string -> string -> unit
    method get_ref : R.key -> (string * string) option
    method get_all : (string * (string * string)) list
  end
type element =
  | H of int * t
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
  | Ref of ref_container * name * t * fallback
  | Img_ref of ref_container * name * alt * fallback
  | Html of name * (string * string option) list * t
  | Html_block of name * (string * string option) list * t
  | Html_comment of string
  | Raw of string
  | Raw_block of string
  | Blockquote of t
  | Img of alt * src * title
  | X of
      < name : string;
        to_html : ?indent:int -> (t -> string) -> t -> string option;
        to_sexpr : (t -> string) -> t -> string option;
        to_t : t -> t option >
and fallback = < to_string : string ; to_t : t >
and name = string
and alt = string
and src = string
and href = string
and title = string
and t = element list

type delim =
  | Ampersand
  | At
  | Backquote
  | Backslash
  | Bar
  | Caret
  | Cbrace
  | Colon
  | Comma
  | Cparenthesis
  | Cbracket
  | Dollar
  | Dot
  | Doublequote
  | Exclamation
  | Equal
  | Greaterthan
  | Hash
  | Lessthan
  | Minus
  | Newline
  | Obrace
  | Oparenthesis
  | Obracket
  | Percent
  | Plus
  | Question
  | Quote
  | Semicolon
  | Slash
  | Space
  | Star
  | Tab
  | Tilde
  | Underscore

type tok =
  | Tag of name * extension
  | Word of string
  | Number of string
  | Delim of int * delim
  (** Lexer's tokens. If you want to use the parser with an extended
      lexer, you may use the constructor [Tag] to implement
      the parser's extension. In the parser, [Tag] is used (at least)
      3 times in order to represent metadata or to store data.

      The integers carried by constructors means that the represented
      character appears (n+2) times. So, [Ampersand(0)] is "&&".
      Notably, this allows to use the property that in the match
      case [Ampersand _ ->], we know there are at least 2 ampersands.
      This is particularly useful for some characters, such as newlines
      and spaces. It's not useful for all of them indeed but it has
      been designed this way for the sake of uniformity (one doesn't
      want to know by heart which constructor have that "at least 2"
      property and which haven't).
  *)

and extension =
  <
    parser_extension : t -> tok list -> tok list -> (t * tok list * tok list) option;
    to_string : string;
  >
(** - [parser_extension] is a method that takes the current state of the
    parser's data and returns None if nothing has been changed,
    otherwise it returns the new state.  The current state of the
    parser's data is [(r, p, l)] where [r] is the result so far, [p]
    is the list of the previous tokens (it's typically empty or
    contains information on how many newlines we've just seen), and
    [l] is the remaining tokens to parse.
    - and [to_string] is a method that returns directly a string
    representation of the object (it's normal if it returns the
    empty string). *)

type extensions = extension list
(** One must use this type to extend the parser. It's a list of
    functions of type [extension]. They are processed in order (the
    head is applied first), so be careful about it. If you use it
    wrong, it will behave wrong. *)

val empty_extension : extension
(** An empty extension *)

val loose_compare : t -> t -> int
(** [loose_compare t1 t2] returns [0] if [t1] and [t2]
    are equivalent, otherwise it returns another number. *)

val normalise_md : t -> t
(** [normalise_md md] returns a copy of [md] where some elements
    have been factorized. *)

val visit : (element -> t option) -> t -> t
(** visitor for structures of type t: [visit f md] will return a new
    potentially altered copy of [md] that has been created by the
    visit of [md] by [f].

    The function [f] takes each [element] (from [md]) and returns
    [Some t] if it has effectively been applied to [element], and
    [None] otherwise. When it returns [Some t], [t] replaces [element]
    in the copy of [md], and when it returns [None], either [element]
    is copied as it is in the copy of [md] or a visited version is
    copied instead (well, that depends on if [element] has elements
    inside of it or not).
*)

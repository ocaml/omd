(** A markdown parser in OCaml, with no extra dependencies.
    N.B. This module is supposed to be reentrant, if it's not then please report the bug. *)

type ref_container
(** abstract type for references container *)

type t = element list
(** Representation of a Markdown document.  *)

(** A element of a Markdown document. *)
and element =
  | Paragraph of t
  | Text of string
  | Emph of t
  | Bold of t
  | Ul of t list
  | Ol of t list
  | Code of string (* html entities are to be converted *later* *)
  | Code_block of string (* html entities are to be converted *later* *)
  | Br
  | Hr
  | Url of href * string * title
  | Ref of ref_container * name * string
  | Img_ref of ref_container * name * alt
  | Html of string
  | Html_block of string
  | H1 of t
  | H2 of t
  | H3 of t
  | H4 of t
  | H5 of t
  | H6 of t
  | Blockquote of t
  | Img of alt * src * title
  | NL

and name = string
(** Markdown reference name. *)

and alt = string
(** HTML img tag attribute. *)

and src = string
(** HTML attribute. *)

and href = string
(** HTML attribute. *)

and title = string
(** HTML attribute. *)


type token
(** Abstract representation of the lexer's tokens *)

val lex : string -> token list
(** Translate a raw string into tokens for the parser *)

type extension =
    Omd_backend.t -> Omd_parser.tag Omd_lexer.t list -> Omd_parser.tag Omd_lexer.t list
    -> ((Omd_backend.t * Omd_parser.tag Omd_lexer.t list * Omd_parser.tag Omd_lexer.t list) option)
  (** A function that takes the current state of the parser's data and
      returns None if nothing has been changed, otherwise it returns
      the new state.  The current state of the parser's data is [(r,
      p, l)] where [r] is the result so far, [p] is the list of the
      previous tokens (it's typically empty or contains information on
      how many newlines we've just seen), and [l] is the remaining
      tokens to parse. *)

and extensions = extension list
(** One must use this type to extend the parser. It's a list of
    functions of type [extension]. They are processed in order (the head is applied first), so
    be careful about it. If you use it wrong, it will behave wrong. *)

val parse : ?extensions:extensions -> token list -> t
(** Translate tokens to Markdown representation *)

val make_paragraphs : t -> t
(** Build Markdown paragraphs. *)

val to_html : t -> string
(** Translate markdown representation into raw HTML.  If you need a
    full HTML representation, you mainly have to figure out how to
    convert [Html of string] into your HTML representation.  *)


;;

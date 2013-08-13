

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

val parse : token list -> t
(** Translate tokens to Markdown representation *)

val make_paragraphs : t -> t
(** Build Markdown paragraphs. *)

val to_html : t -> string
(** Translate markdown representation into raw HTML.  If you need a
    full HTML representation, you mainly have to figure out how to
    convert [Html of string] into your HTML representation.  *)

;;

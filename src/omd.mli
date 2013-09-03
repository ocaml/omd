(** A markdown parser in OCaml, with no extra dependencies.
    
    This module represents this entire Markdown library written in
    OCaml only. 

    Its main purpose is to allow you to use the Markdown library while
    keeping you away from the other modules. 

    If you want to extend the Markdown parser, you can do it without
    accessing any module of this library but this one, and by doing
    so, you are free from having to maintain a fork of this library.

    N.B. This module is supposed to be reentrant, 
    if it's not then please report the bug. *)

(********************** TYPES **************************************)
type t = Omd_representation.t
(** Representation of a Markdown document.  *)

and ref_container =
    (< add_ref: string -> string -> string -> unit ; 
     get_ref : string -> (string*string); >)

(** A element of a Markdown document. *)
and element = Omd_representation.element =
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
  | X of (< (* extension of [element]. *) 
           name: string;
           (* N.B. [to_html] means that htmlentities will not
              be applied to its output. *)
           to_html: ?indent:int -> (t -> string) -> t -> string option;
           to_sexpr: (t -> string) -> t -> string option;
           to_t: t -> t option >)

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

type tok = Omd_representation.tok =
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
(** Lexer's tokens. If you want to use the parser with an extended
    lexer, you may use the constructor [Tag] to implement
    the parser's extension. In the parser, [Tag] is used (at least) 
    3 times in order to represent metadata or to store data. *)

  (** A function that takes the current state of the parser's data and
      returns None if nothing has been changed, otherwise it returns
      the new state.  The current state of the parser's data is [(r,
      p, l)] where [r] is the result so far, [p] is the list of the
      previous tokens (it's typically empty or contains information on
      how many newlines we've just seen), and [l] is the remaining
      tokens to parse. *)


(** One must use this type to extend the parser. It's a list of
    functions of type [extension]. They are processed in order (the
    head is applied first), so be careful about it. If you use it
    wrong, it will behave wrong. *)

and extensions = extension list
and extension = (t -> tok list -> tok list -> ((t * tok list * tok list) option)) 



(********************** VALUES **************************************)

val lex : string -> tok list
(** Translate a raw string into tokens for the parser.  To implement
    an extension to the lexer, one may process its result before
    giving it to the parser. To implement an extension to the 
    parser, one may extend it using the constructor [Tag]
    from type [tok] and/or using the extensions mechanism
    of the parser (cf. the optional argument [extensions]).
    The main difference is that [Tag] is processed by the parser
    in highest priority whereas functions in [extensions] are applied
    with lowest priority. *)


val parse : ?extensions:extensions -> tok list -> t
(** Translate tokens to Markdown representation *)

val make_paragraphs : t -> t
(** Build Markdown paragraphs. This Markdown parser doesn't
    build paragraph directly, one has to call this function
    to build them. On the other hand, if you don't want
    automatic Markdown-style paragraphs, don't call this function! *)

val to_html : ?pindent:bool -> ?nl2br:bool -> t -> string
(** Translate markdown representation into raw HTML.  If you need a
    full HTML representation, you mainly have to figure out how to
    convert [Html of string] and [Html_block of string]
    into your HTML representation.  *)

val to_markdown : t -> string
(** Translate markdown representation into textual markdown. *)

val to_text : t -> string
(** Translate markdown representation into raw text. *)


;;

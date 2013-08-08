(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

module Md : sig
  (** abstract type for references container *)
  type ref_container

  (** A element of a Markdown document *)
  type md_element = 
    | Paragraph of md
    | Text of string
    | Emph of md
    | Bold of md
    | Ul of li list
    | Ol of li list
    | Code of string (* html entities are to be converted *later* *)
    | Br
    | Hr
    | Url of href * string * title
    | Ref of ref_container * name * string
    | Html of string
    | H1 of md
    | H2 of md
    | H3 of md
    | H4 of md
    | H5 of md
    | H6 of md
    | Blockquote of md
    | Img of alt * src * title
    | NL

  (** markdown reference name *)
  and name = string

  (** html img tag attribute *)
  and alt = string

  (** html attribute *)
  and src = string

  (** html attribute *)
  and href = string

  (** html attribute *)
  and title = string

  (** element of a list *)
  and li = Li of md

  (** Representation of a Markdown document.  *) 
  and md = md_element list

  (** Abstract representation of the lexer's tokens *)
  type token

  (** Translate a raw string into tokens for the parser *)
  val lex : string -> token list

  (** Translate tokens to Markdown representation *)
  val parse : token list -> md

  (** Build Markdown paragraphs. *)
  val make_paragraphs : md -> md
    
  (** Translate markdown representation into raw HTML *)
  val html_of_md : md -> string

(** If you need a full HTML representation, you mainly have to figure out
    how to convert [Html of string] into your HTML representation. *)
end =
struct
  include Md_backend

  include Md_lexer
    
  include Md_parser

  type token = tag Md_lexer.t

  let lex : string -> token list = lex

  let parse : token list -> md = parse

  let html_of_md : md -> string = html_of_md

  let html_of_string (html:string) : string =
    html_of_md (parse (lex html))

end




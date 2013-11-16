(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

include Omd_representation
include Omd_backend

let of_string ?extensions ?(paragraph=false) s =
  let md = Omd_parser.parse ?extensions (Omd_lexer.lex s) in
  if paragraph then make_paragraphs md else md


let to_html : ?pindent:bool -> ?nl2br:bool -> ?cs:code_stylist -> t -> string =
  html_of_md

let to_text : t -> string = text_of_md

let to_markdown : t -> string = markdown_of_md

let html_of_string (html:string) : string =
  html_of_md (Omd_parser.parse (Omd_lexer.lex html))


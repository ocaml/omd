(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

include Omd_backend

type extension = Omd_parser.extension 
and extensions = Omd_parser.extensions

type token = Omd_parser.tag Omd_lexer.t

let lex : string -> token list = Omd_lexer.lex

let parse : ?extensions:extensions -> token list -> t = Omd_parser.parse

let to_html : t -> string = html_of_md

let html_of_string (html:string) : string =
  html_of_md (parse (lex html))





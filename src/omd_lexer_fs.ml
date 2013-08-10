(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

(** You should either use this module or OMd_lexer, not both.
    This module includes OMd_lexer.
*)

include OMd_lexer

let lex_from_inchannel ic =
  (* Maintenance-easiness-driven implementation. *)
  let ic_content =
    let b = Buffer.create 42 in
    try while true do
          Buffer.add_char b (input_char ic)
        done;
        assert false
    with End_of_file -> Buffer.contents b in
  lex ic_content

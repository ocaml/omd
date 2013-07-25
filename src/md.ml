(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

type md_element = 
  | Paragraph of md
  | Text of string
  | Emph of md
  | Bold of md
  | Ul of li list
  | Ol of li list
  | Sp of int (* spaces *)
  | Code of string (* html entities are to be converted *later* *)
  | Br
  | Hr
  | Html of string
and li = Li of md | Li2 of md
and md = md_element list
    

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

let () =
  let b = Buffer.create 42 in
    try while true do
      Buffer.add_char b (input_char stdin)
    done; assert false
    with End_of_file ->
      let open Md_lib.Md in
      print_endline
        (
          html_of_md 
            (make_paragraphs
               (parse (lex (Buffer.contents b)))
            )
        )


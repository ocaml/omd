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
      let open Omd in
      print_endline (Omd.to_html
                       (make_paragraphs
                          (parse (lex (Buffer.contents b)))));
      if try ignore (Sys.getenv "DEBUG"); true with _ -> false then
        print_endline
          (Omd_backend.sexpr_of_md
             ((* Md_backend.make_paragraphs *)
               (Omd_parser.parse (Omd_lexer.lex (Buffer.contents b)))))

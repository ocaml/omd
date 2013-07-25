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
and li = Li of md
and md = md_element list

let htmlentities s = s

let html_of_md md = 
  let b = Buffer.create 42 in
  let rec loop = function
    | Paragraph md ->
        Buffer.add_string b "<p>";
        List.iter loop md;
        Buffer.add_string b "</p>"
    | Text t ->
        Buffer.add_string b t
    | Emph md ->
        Buffer.add_string b "<em>";
        List.iter loop md;
        Buffer.add_string b "</em>"
    | Bold md ->
        Buffer.add_string b "<strong>";
        List.iter loop md;
        Buffer.add_string b "</strong>"
    | Ul l ->
        Buffer.add_string b "<ul>";
        List.iter
          (fun (Li li) ->
             Buffer.add_string b "<li>";
             List.iter loop li;
             Buffer.add_string b "</li>")
          l;
          Buffer.add_string b "</ul>"
    | Ol l ->
        Buffer.add_string b "<ol>";
        List.iter
          (fun (Li li) ->
             Buffer.add_string b "<li>";
             List.iter loop li;
             Buffer.add_string b "</li>")
          l;
          Buffer.add_string b "</ol>"
    | Sp n ->
        for i = 0 to n do
          Buffer.add_char b ' '
        done
    | Code c ->
        Buffer.add_string b "<pre>";
        Buffer.add_string b (htmlentities c);
        Buffer.add_string b "</pre>"
    | Br ->
        Buffer.add_string b "<br/>"
    | Hr ->
        Buffer.add_string b "<hr/>"
    | Html s ->
        Buffer.add_string b s
  in 
    List.iter loop md;
    Buffer.contents b


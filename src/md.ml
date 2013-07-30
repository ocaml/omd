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
  | Code of string (* html entities are to be converted *later* *)
  | Br
  | Hr
  | Url of href * string * title
  | Html of string
  | H1 of md
  | H2 of md
  | H3 of md
  | H4 of md
  | H5 of md
  | H6 of md
  | NL
and href = string
and title = string
and li = Li of md
and md = md_element list

let htmlentities s =
  let b = Buffer.create 42 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | ( '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' ) as c -> Buffer.add_char b c
        | '"' -> Buffer.add_string b "&quot;"
        | '\'' -> Buffer.add_string b "&apos;"
        | '&' -> Buffer.add_string b "&amp;"
        | '<' -> Buffer.add_string b "&lt;"
        | '>' -> Buffer.add_string b "&gt;"
        | '\\' -> Buffer.add_string b "&#92;"
        | c -> Buffer.add_char b c
    done;
    Buffer.contents b

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
    | Url (href,s,title) ->
        let s = htmlentities s in
          Buffer.add_string b "<a href='";
          Buffer.add_string b (htmlentities href);
          Buffer.add_string b "'";
          if title <> "" then 
            begin
              Buffer.add_string b " title='";
              Buffer.add_string b (htmlentities title);
              Buffer.add_string b "'";
            end;
          Buffer.add_string b ">";
          Buffer.add_string b s;
          Buffer.add_string b "</a>";
    | H1 md ->
        Buffer.add_string b "<h1>";
        List.iter loop md;
        Buffer.add_string b "</h1>"
    | H2 md ->
        Buffer.add_string b "<h2>";
        List.iter loop md;
        Buffer.add_string b "</h2>"
    | H3 md ->
        Buffer.add_string b "<h3>";
        List.iter loop md;
        Buffer.add_string b "</h3>"
    | H4 md ->
        Buffer.add_string b "<h4>";
        List.iter loop md;
        Buffer.add_string b "</h4>"
    | H5 md ->
        Buffer.add_string b "<h5>";
        List.iter loop md;
        Buffer.add_string b "</h5>"
    | H6 md ->
        Buffer.add_string b "<h6>";
        List.iter loop md;
        Buffer.add_string b "</h6>"
    | NL ->
        Buffer.add_char b '\n'
  in 
    List.iter loop md;
    Buffer.contents b


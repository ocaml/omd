(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

let pindent = false
let pindent = true
let smdnl = true (* about standard markdown new lines *)


(** references *)
module R = Map.Make(String)
class ref_container = object
  val broken_url = "", "Broken URL"
  val mutable c = R.empty
  method add_ref name title url =
    c <- R.add name (url, title) c
  method get_ref name =
    let (url, title) as r = 
      try R.find name c
      with Not_found -> broken_url
    in r
end

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
  | Quote of md
  | Img of alt * src * title
  | NL
and name = string
and alt = string
and src = string
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


let make_paragraphs md =
  let rec remove_prefix prefix = function
    | [] ->
        []
    | e::tl as l ->
        if e = prefix then
          remove_prefix prefix tl
        else
          l
  in
  let rec loop cp accu = function (* cp means current paragraph *)
    | [] ->
        let accu =
          if cp = [] || cp = [NL] then
            accu
          else
            Paragraph(List.rev cp)::accu
        in
          List.rev accu
    | (Code _ | H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Html _ | Br | Hr) as e :: tl->
        if cp = [] || cp = [NL] then 
          loop cp (e::accu) tl
        else
          loop [] (e::Paragraph(List.rev cp)::accu) tl
    | Text "\n" :: _ | Paragraph _ :: _ ->
        assert false
    | NL::NL::tl ->
        let tl = remove_prefix NL tl in
          begin match cp with
            | [] | [NL] ->
                loop [] (NL::accu) tl
            | _ ->
                loop [] (Paragraph(List.rev cp)::accu) tl
          end
    | x::tl ->
        loop (x::cp) accu tl
  in
    loop [] [] md

(* let make_paragraphs x = x  *)


let rec html_of_md md = 
  let empty s =
    let rec loop i =
      if i < String.length s then
        match s.[i] with
          | ' ' | '\n' -> loop (i+1)
          | _ -> false
      else
        true
    in
      loop 0
  in    
  let b = Buffer.create 42 in
  let rec loop indent = function
    | Ref(rc, name, text) :: tl ->
        let title, href =
          rc#get_ref name
        in
        loop indent (Url(href,text,title)::tl)
    | Paragraph md :: tl ->
        (let s = html_of_md md in
           if empty s then
             ()
           else
             begin
               Buffer.add_string b "<p>";
               Buffer.add_string b s;
               Buffer.add_string b "</p>\n";
             end);
        loop indent tl
    | Img(alt, src, title) :: tl ->
        Buffer.add_string b "<img src='";
        Buffer.add_string b (htmlentities src);
        Buffer.add_string b "' alt='";
        Buffer.add_string b (htmlentities alt);
        Buffer.add_string b "' ";
        if title <> "" then
          (Buffer.add_string b "' title='";
           Buffer.add_string b (htmlentities title);
           Buffer.add_string b "' ");
        Buffer.add_string b "/>";
        loop indent tl
    | Text t :: tl ->
        (* Buffer.add_string b t; *)
        Buffer.add_string b (htmlentities t);
        loop indent tl
    | Emph md :: tl ->
        Buffer.add_string b "<em>";
        loop indent md;
        Buffer.add_string b "</em>";
        loop indent tl
    | Bold md :: tl ->
        Buffer.add_string b "<strong>";
        loop indent md;
        Buffer.add_string b "</strong>";
        loop indent tl
    | Ul l :: tl ->
        if pindent then Buffer.add_char b '\n';
        if pindent then for i = 0 to indent do Buffer.add_char b ' ' done;
        Buffer.add_string b "<ul>";
        if pindent then Buffer.add_char b '\n';
        List.iter
          (fun (Li li) ->
            if pindent then for i = 0 to indent + 1 do Buffer.add_char b ' ' done;
            Buffer.add_string b "<li>";
            loop (indent+2) li;
            Buffer.add_string b "</li>";
            if pindent then Buffer.add_char b '\n')
          l;
        if pindent then for i = 0 to indent do Buffer.add_char b ' ' done;
        Buffer.add_string b "</ul>";
        if pindent then Buffer.add_char b '\n';
        loop indent tl
    | Ol l :: tl ->
        if pindent then Buffer.add_char b '\n';
        if pindent then for i = 0 to indent do Buffer.add_char b ' ' done;
        Buffer.add_string b "<ol>";
        if pindent then Buffer.add_char b '\n';
        List.iter
          (fun (Li li) ->
            if pindent then for i = 0 to indent + 1 do Buffer.add_char b ' ' done;
            Buffer.add_string b "<li>";
            loop (indent+2) li;
            Buffer.add_string b "</li>";
            if pindent then Buffer.add_char b '\n')
          l;
        if pindent then for i = 0 to indent do Buffer.add_char b ' ' done;
        Buffer.add_string b "</ol>";
        if pindent then Buffer.add_char b '\n';
        loop indent tl
    | Code c :: tl ->
        Buffer.add_string b "<pre>";
        Buffer.add_string b (htmlentities c);
        Buffer.add_string b "</pre>\n";
        loop indent tl
    | Br :: tl ->
        Buffer.add_string b "<br/>\n";
        loop indent tl
    | Hr :: tl ->
        Buffer.add_string b "<hr/>\n";
        loop indent tl
    | Html s :: tl ->
        Buffer.add_string b s;
        loop indent tl
    | Url (href,s,title) :: tl ->
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
          loop indent tl
    | H1 md :: tl ->
        Buffer.add_string b "<h1>";
        loop indent md;
        Buffer.add_string b "</h1>";
        loop indent tl
    | H2 md :: tl ->
        Buffer.add_string b "<h2>";
        loop indent md;
        Buffer.add_string b "</h2>";
        loop indent tl
    | H3 md :: tl ->
        Buffer.add_string b "<h3>";
        loop indent md;
        Buffer.add_string b "</h3>";
        loop indent tl
    | H4 md :: tl ->
        Buffer.add_string b "<h4>";
        loop indent md;
        Buffer.add_string b "</h4>";
        loop indent tl
    | H5 md :: tl ->
        Buffer.add_string b "<h5>";
        loop indent md;
        Buffer.add_string b "</h5>";
        loop indent tl
    | H6 md :: tl ->
        Buffer.add_string b "<h6>";
        loop indent md;
        Buffer.add_string b "</h6>";
        loop indent tl
    | NL :: tl ->
        if smdnl then Buffer.add_string b "<br />";
        Buffer.add_char b '\n';
        loop indent tl
    | [] -> ()
  in 
    loop 0 md;
    Buffer.contents b


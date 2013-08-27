(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Printf
open Omd_representation
open Omd_utils

let pindent = false
let pindent = true
let smdnl = true (* about standard markdown new lines *)


let id_of_string ids s =
  let l = String.length s in
  let gen_id s =
    let b = Buffer.create l in
    let rec loop i flag flag2 =
      (* [flag] prevents trailing dashes; 
         [flag2] prevents IDs from starting with dashes *)
      if i = l then
        ()
      else
        match s.[i] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c ->
          (if not (flag2 || flag) then Buffer.add_char b '-');
          Buffer.add_char b c;
          loop (i+1) true true
        | _ ->
          if flag2 || flag then
            loop (i+1) false flag2
          else
            (Buffer.add_char b '-';
             loop (i+1) true flag2)
    in
    loop 0 true true;
    Buffer.contents b
  in
  let id = gen_id s in
  ids#mangle id



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


(** - recognizes paragraphs - glues following blockquotes  *)
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
    | Blockquote b1 :: Blockquote b2 :: tl
    | Blockquote b1 :: NL :: Blockquote b2 :: tl
    | Blockquote b1 :: NL :: NL :: Blockquote b2 :: tl ->
        loop cp accu (Blockquote(b1@b2):: tl)
    | Blockquote b :: tl ->
        let e = Blockquote(loop [] [] b) in
          if cp = [] || cp = [NL] then
            loop cp (e::accu) tl
          else
            loop [] (e::Paragraph(List.rev cp)::accu) tl
    | (Ul b) :: tl ->
        let e = Ul(List.map (fun li -> loop [] [] li) b) in
        if cp = [] || cp = [NL] then
          loop cp (e::accu) tl
        else
          loop [] (e::Paragraph(List.rev cp)::accu) tl
    | (Ol b) :: tl ->
        let e = Ol(List.map (fun li -> loop [] [] li) b) in
        if cp = [] || cp = [NL] then
          loop cp (e::accu) tl
        else
          loop [] (e::Paragraph(List.rev cp)::accu) tl
    | Html_comments _ as e :: tl ->
      if cp = [] then
        loop cp (e::accu) tl
      else
        loop (e::cp) accu tl
    | (Code_block _ | H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _
       | Html_block _) as e :: tl->
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

let text_of_md md =
  let b = Buffer.create 42 in
  let rec loop = function
    | X _ :: tl ->
        loop tl
    | Blockquote q :: tl ->
        loop q;
        loop tl
    | Ref(rc, name, text) :: tl ->
        Buffer.add_string b (htmlentities name);
        loop tl
    | Img_ref(rc, name, alt) :: tl ->
        Buffer.add_string b (htmlentities name);
        loop tl
    | Paragraph md :: tl ->
        loop md;
        loop tl
    | Img(alt, src, title) :: tl ->
        Buffer.add_string b (htmlentities alt);
        loop tl
    | Text t :: tl ->
        Buffer.add_string b (htmlentities t);
        loop tl
    | Emph md :: tl ->
        loop md;
        loop tl
    | Bold md :: tl ->
        loop md;
        loop tl
    | Ul l :: tl ->
        List.iter loop l;
        loop tl
    | Ol l :: tl ->
        List.iter loop l;
        loop tl
    | Code_block c :: tl ->
        Buffer.add_string b (htmlentities c);
        loop tl
    | Code c :: tl ->
        Buffer.add_string b (htmlentities c);
        loop tl
    | Br :: tl ->
        loop tl
    | Hr :: tl ->
        loop tl
    | Html s :: tl ->
        loop tl
    | Html_block s :: tl ->
        loop tl
    | Html_comments s :: tl ->
        loop tl
    | Url (href,s,title) :: tl ->
        loop s;
        loop tl
    | H1 md :: tl 
    | H2 md :: tl
    | H3 md :: tl
    | H4 md :: tl
    | H5 md :: tl
    | H6 md :: tl ->
        loop md;
        loop tl
    | NL :: tl ->
        loop tl
    | [] -> ()
  in
    loop md;
    Buffer.contents b


let rec html_and_headers_of_md md =
  let ids = object(this)
    val mutable ids = StringSet.empty
    method mangle id =
      if StringSet.mem id ids then
        this#mangle (id^"x")
      else
        (ids <- StringSet.add id ids;
         id)
  end in
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
  let headers = ref [] in
  let rec loop indent = function
    | X x :: tl ->
        (match x#to_t md with
           | Some t -> loop indent t
           | None ->
               match x#to_html ~indent:indent html_of_md md with
                 | Some s -> Buffer.add_string b s
                 | None -> ());
        loop indent tl
    | Blockquote q :: tl ->
        Buffer.add_string b "<blockquote>";
        loop indent q;
        Buffer.add_string b "</blockquote>";
        loop indent tl
    | Ref(rc, name, text) :: tl ->
        let href, title =
          rc#get_ref name
        in
        loop indent
          (Url(htmlentities href,
              [Text(text)],
              htmlentities title)
            ::tl)
    | Img_ref(rc, name, alt) :: tl ->
        let src, title =
          rc#get_ref name
        in
        loop indent
          (Img(htmlentities alt,htmlentities src,htmlentities title)::tl)
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
          (Buffer.add_string b " title='";
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
          (fun li ->
            if pindent then for i = 0 to indent + 1 do
                              Buffer.add_char b ' '
                            done;
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
          (fun li ->
            if pindent then for i = 0 to indent + 1 do
                              Buffer.add_char b ' '
                            done;
            Buffer.add_string b "<li>";
            loop (indent+2) li;
            Buffer.add_string b "</li>";
            if pindent then Buffer.add_char b '\n')
          l;
        if pindent then for i = 0 to indent do Buffer.add_char b ' ' done;
        Buffer.add_string b "</ol>";
        if pindent then Buffer.add_char b '\n';
        loop indent tl
    | Code_block c :: tl ->
        Buffer.add_string b "<pre><code>";
        Buffer.add_string b (htmlentities c);
        Buffer.add_string b "</code></pre>\n";
        loop indent tl
    | Code c :: tl ->
        Buffer.add_string b "<code>";
        Buffer.add_string b (htmlentities c);
        Buffer.add_string b "</code>";
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
    | Html_block s :: tl ->
        Buffer.add_string b s;
        loop indent tl
    | Html_comments s :: tl ->
        Buffer.add_string b s;
        loop indent tl
    | Url (href,s,title) :: tl ->
        let s = html_of_md s in
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
    | (H1 md as e) :: tl ->
      let ih = html_of_md md in
      let id = id_of_string ids (text_of_md md) in
      headers := (e, id, ih) :: !headers;
      Buffer.add_string b "<h1 id=\"";
      Buffer.add_string b id;
      Buffer.add_string b "\">";
      Buffer.add_string b ih;
      Buffer.add_string b "</h1>";
      loop indent tl
    | (H2 md as e) :: tl ->
      let ih = html_of_md md in
      let id = id_of_string ids (text_of_md md) in
      headers := (e, id, ih) :: !headers;
      Buffer.add_string b "<h2 id=\"";
      Buffer.add_string b id;
      Buffer.add_string b "\">";
      Buffer.add_string b ih;
      Buffer.add_string b "</h2>";
      loop indent tl
    | (H3 md as e) :: tl ->
      let ih = html_of_md md in
      let id = id_of_string ids (text_of_md md) in
      headers := (e, id, ih) :: !headers;
      Buffer.add_string b "<h3 id=\"";
      Buffer.add_string b id;
      Buffer.add_string b "\">";
      Buffer.add_string b ih;
      Buffer.add_string b "</h3>";
      loop indent tl
    | (H4 md as e) :: tl ->
      let ih = html_of_md md in
      let id = id_of_string ids (text_of_md md) in
      headers := (e, id, ih) :: !headers;
      Buffer.add_string b "<h4 id=\"";
      Buffer.add_string b id;
      Buffer.add_string b "\">";
      Buffer.add_string b ih;
      Buffer.add_string b "</h4>";
      loop indent tl
    | (H5 md as e) :: tl ->
      let ih = html_of_md md in
      let id = id_of_string ids (text_of_md md) in
      headers := (e, id, ih) :: !headers;
      Buffer.add_string b "<h5 id=\"";
      Buffer.add_string b id;
      Buffer.add_string b "\">";
      Buffer.add_string b ih;
      Buffer.add_string b "</h5>";
      loop indent tl
    | (H6 md as e) :: tl ->
      let ih = html_of_md md in
      let id = id_of_string ids (text_of_md md) in
      headers := (e, id, ih) :: !headers;
      Buffer.add_string b "<h6 id=\"";
      Buffer.add_string b id;
      Buffer.add_string b "\">";
      Buffer.add_string b ih;
      Buffer.add_string b "</h6>";
      loop indent tl
    | NL :: tl ->
        if not(smdnl) then Buffer.add_string b "<br />";
        Buffer.add_char b '\n';
        loop indent tl
    | [] -> ()
  in
    loop 0 md;
    Buffer.contents b, List.rev !headers

and html_of_md md =
  fst (html_and_headers_of_md md)
and headers_of_md md =
  snd (html_and_headers_of_md md)


let rec sexpr_of_md md =
  let b = Buffer.create 42 in
  let rec loop = function
    | X x :: tl ->

        (match x#to_t md with
           | Some t -> loop t
           | None ->
               match x#to_sexpr sexpr_of_md md with
                 | Some s -> Buffer.add_string b s
                 | None ->
                     match x#to_html ~indent:0 html_of_md md with
                       | Some s -> Buffer.add_string b s
                       | None -> ());
        loop tl
    | Blockquote q :: tl ->
        Buffer.add_string b "(Blockquote";
        loop q;
        Buffer.add_string b ")";
        loop tl
    | Ref(rc, name, text) :: tl ->
        bprintf b "(Ref %S %S)" name text;
        loop tl
    | Img_ref(rc, name, alt) :: tl ->
        bprintf b "(Img_ref %S %S)" name alt;
        loop tl
    | Paragraph md :: tl ->
        Buffer.add_string b "(Paragraph";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | Img(alt, src, title) :: tl ->
        bprintf b "(Img %S %S %S)" alt src title;
        loop tl
    | Text t1 :: Text t2 :: tl ->
        loop (Text(t1^t2)::tl)
    | Text t :: tl ->
        bprintf b "(Text %S)" t;
        loop tl
    | Emph md :: tl ->
        Buffer.add_string b "(Emph";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | Bold md :: tl ->
        Buffer.add_string b "(Bold";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | Ol l :: tl ->
        bprintf b "(Ol";
        List.iter(fun li -> bprintf b "(Li "; loop li; bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Ul l :: tl ->
        bprintf b "(Ul";
        List.iter(fun li -> bprintf b "(Li "; loop li;bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Code c :: tl ->
        bprintf b "(Code %S)" c;
        loop tl
    | Code_block c :: tl ->
        bprintf b "(Code_block %s)" c;
        loop tl
    | Br :: tl ->
        Buffer.add_string b "(Br)";
        loop tl
    | Hr :: tl ->
        Buffer.add_string b "(Hr)";
        loop tl
    | Html s :: tl ->
        Buffer.add_string b "(Html ";
        Buffer.add_string b s;
        Buffer.add_string b ")";
        loop tl
    | Html_block s :: tl ->
        Buffer.add_string b "(Html_block ";
        Buffer.add_string b s;
        Buffer.add_string b ")";
        loop tl
    | Html_comments s :: tl ->
        Buffer.add_string b "(Html_comments ";
        Buffer.add_string b s;
        Buffer.add_string b ")";
        loop tl
    | Url (href,s,title) :: tl ->
        bprintf b "(Url %S %s %S)" href (html_of_md s) title;
        loop tl
    | H1 md :: tl ->
        Buffer.add_string b "(H1";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H2 md :: tl ->
        Buffer.add_string b "(H2";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H3 md :: tl ->
        Buffer.add_string b "(H3";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H4 md :: tl ->
        Buffer.add_string b "(H4";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H5 md :: tl ->
        Buffer.add_string b "(H5";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | H6 md :: tl ->
        Buffer.add_string b "(H6";
        loop md;
        Buffer.add_string b ")";
        loop tl
    | NL :: tl ->
        Buffer.add_string b "(NL)";
        loop tl
    | [] -> ()
  in
    loop md;
    Buffer.contents b

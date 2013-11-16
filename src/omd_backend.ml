(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

type code_stylist = < style : lang:string -> string -> string >

open Printf
open Omd_representation
open Omd_utils

let default_language = ref ""

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
    | (Ulp b) :: tl ->
        let e = Ul(List.map (fun li -> loop [] [] li) b) in
        if cp = [] || cp = [NL] then
          loop cp (e::accu) tl
        else
          loop [] (e::Paragraph(List.rev cp)::accu) tl
    | (Olp b) :: tl ->
        let e = Ol(List.map (fun li -> loop [] [] li) b) in
        if cp = [] || cp = [NL] then
          loop cp (e::accu) tl
        else
          loop [] (e::Paragraph(List.rev cp)::accu) tl
    | Html_comment _ as e :: tl ->
      if cp = [] then
        loop [] (e::accu) tl
      else if cp = [NL] then
        loop [] (e::NL::accu) tl
      else
        loop (e::cp) accu tl
    | (Code_block _ | H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Ol _ | Ul _
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
    | Ref(rc, name, text, fallback) :: tl ->
        Buffer.add_string b (htmlentities ~md:true name);
        loop tl
    | Img_ref(rc, name, alt, fallback) :: tl ->
        Buffer.add_string b (htmlentities ~md:true name);
        loop tl
    | Paragraph md :: tl ->
        loop md;
        loop tl
    | Img(alt, src, title) :: tl ->
        Buffer.add_string b (htmlentities ~md:true alt);
        loop tl
    | Text t :: tl ->
        Buffer.add_string b (htmlentities ~md:true t);
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
    | Ulp l :: tl ->
        List.iter loop l;
        loop tl
    | Olp l :: tl ->
        List.iter loop l;
        loop tl
    | Code_block(lang, c) :: tl ->
        Buffer.add_string b (htmlentities ~md:false c);
        loop tl
    | Code(lang, c) :: tl ->
        Buffer.add_string b (htmlentities ~md:false c);
        loop tl
    | Br :: tl ->
        loop tl
    | Hr :: tl ->
        loop tl
    | Html s :: tl ->
        loop tl
    | Html_block s :: tl ->
        loop tl
    | Html_comment s :: tl ->
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

let default_code_stylist = object method style ~lang code = code end

let rec html_and_headers_of_md ?(pindent=true) ?(nl2br=false) ?(cs=default_code_stylist) md =
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
  let remove_trailing_blanks s =
    let rec loop i =
      if i < 0 then ""
      else
        match s.[i] with
        | ' '|'\t'|'\n' ->
          loop (pred i)
        | _ ->
          if i = String.length s - 1 then
            s
          else
            String.sub s 0 (i+1)
    in loop (String.length s - 1)
  in
  let b = Buffer.create 42 in
  let headers = ref [] in
  let rec loop indent ?(nl=false) = function
    | X x :: tl ->
        (match x#to_t md with
           | Some t -> loop indent t
           | None ->
               match x#to_html ~indent:indent html_of_md md with
                 | Some s -> Buffer.add_string b s
                 | None -> ());
        loop indent tl
    | Blockquote q :: tl ->
      if nl then Buffer.add_string b "\n";
      Buffer.add_string b "<blockquote>";
      loop indent q;
      Buffer.add_string b "</blockquote>";
      loop indent ~nl:true tl
    | Ref(rc, name, text, fallback) :: tl ->
      begin match rc#get_ref name with
        | Some(href, title) ->
          loop indent
            (Url(htmlentities ~md:true href,
                 [Text(text)],
                 htmlentities ~md:true title)
             ::tl)
        | None -> loop indent (Text(fallback)::tl)
      end
    | Img_ref(rc, name, alt, fallback) :: tl ->
        begin match rc#get_ref name with
          | Some(src, title) ->
            loop indent
              (Img(htmlentities ~md:true alt,
                   htmlentities ~md:true src,
                   htmlentities ~md:true title)::tl)
          | None -> loop indent (Text(fallback)::tl)
        end
    | Paragraph md :: tl ->
        (let s = html_of_md md in
           if empty s then
             ()
           else
             begin
               if nl then Buffer.add_string b "\n";
               Buffer.add_string b "<p>";
               Buffer.add_string b (remove_trailing_blanks s);
               Buffer.add_string b "</p>";
             end);
        loop indent ~nl:true tl
    | Img(alt, src, title) :: tl ->
        Buffer.add_string b "<img src='";
        Buffer.add_string b (htmlentities ~md:true src);
        Buffer.add_string b "' alt='";
        Buffer.add_string b (htmlentities ~md:true alt);
        Buffer.add_string b "' ";
        if title <> "" then
          (Buffer.add_string b " title='";
           Buffer.add_string b (htmlentities ~md:true title);
           Buffer.add_string b "' ");
        Buffer.add_string b "/>";
        loop indent tl
    | Text t :: tl ->
        (* Buffer.add_string b t; *)
        Buffer.add_string b (htmlentities ~md:true t);
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
    | (Ul l|Ol l|Ulp l|Olp l as e) :: tl ->
      if nl then Buffer.add_string b "\n";
      (* if pindent then Buffer.add_char b '\n'; *)
      if pindent then for i = 1 to indent do Buffer.add_char b ' ' done;
      (match e with
       | Ol _|Olp _ ->
         Buffer.add_string b "<ol>";
       | _ ->
         Buffer.add_string b "<ul>");
      if pindent then Buffer.add_char b '\n';
      List.iter
        (fun li ->
           if pindent then for i = 1 to indent + 1 do
               Buffer.add_char b ' '
             done;
           Buffer.add_string b "<li>";
           loop (indent+2) li;
           if (try Buffer.nth b (Buffer.length b - 1) = '\n' with _ -> false)
           then
             if pindent then for i = 1 to indent + 1 do
                 Buffer.add_char b ' '
               done;
           Buffer.add_string b "</li>";
           if pindent then Buffer.add_char b '\n')
        l;
      if pindent then for i = 1 to indent do Buffer.add_char b ' ' done;
      (match e with
       | Ol _|Olp _ ->
         Buffer.add_string b "</ol>";
       | _ ->
         Buffer.add_string b "</ul>");
      if pindent then Buffer.add_char b '\n';
      loop indent tl
    | Code_block(lang, c) :: tl ->
      if nl then Buffer.add_string b "\n";
      if lang = "" && !default_language = "" then
        Buffer.add_string b "<pre><code>"
      else if lang = "" then
        bprintf b "<pre><code class='%s'>" !default_language
      else
        bprintf b "<pre><code class='%s'>" lang;
      let new_c = cs#style ~lang:lang c in
      if c = new_c then
        Buffer.add_string b (htmlentities ~md:false c)
      else
        Buffer.add_string b new_c;
      Buffer.add_string b "</code></pre>";
      loop indent ~nl:true tl
    | Code(lang, c) :: tl ->
      Buffer.add_string b "<code>";
      Buffer.add_string b (htmlentities ~md:false c);
      Buffer.add_string b "</code>";
      loop indent tl
    | Br :: tl ->
      if nl then Buffer.add_string b "\n";
      Buffer.add_string b "<br/>\n";
      loop indent tl
    | Hr :: tl ->
      if nl then Buffer.add_string b "\n";
      Buffer.add_string b "<hr/>\n";
      loop indent tl
    | Html s :: tl ->
      Buffer.add_string b s;
      loop indent tl
    | Html_block s :: tl ->
      if nl then Buffer.add_string b "\n";
      Buffer.add_string b s;
      loop indent ~nl:true tl
    | Html_comment s :: tl ->
      if nl then Buffer.add_string b "\n";
      Buffer.add_string b s;
      loop indent ~nl:nl tl
    | Url (href,s,title) :: tl ->
      let s = html_of_md s in
      Buffer.add_string b "<a href='";
      Buffer.add_string b (htmlentities ~md:true href);
      Buffer.add_string b "'";
      if title <> "" then
        begin
          Buffer.add_string b " title='";
          Buffer.add_string b (htmlentities ~md:true title);
          Buffer.add_string b "'";
        end;
      Buffer.add_string b ">";
      Buffer.add_string b s;
      Buffer.add_string b "</a>";
      loop indent tl
    | (H1 md as e) :: tl ->
      if nl then Buffer.add_string b "\n";
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
      if nl then Buffer.add_string b "\n";
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
      if nl then Buffer.add_string b "\n";
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
      if nl then Buffer.add_string b "\n";
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
      if nl then Buffer.add_string b "\n";
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
      if nl then Buffer.add_string b "\n";
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
        if nl2br then Buffer.add_string b "<br />";
        Buffer.add_char b '\n';
        loop indent tl
    | [] ->
      if nl then Buffer.add_string b "\n"
  in
    loop 0 md;
    Buffer.contents b, List.rev !headers

and html_of_md ?(pindent=true) ?(nl2br=false) ?(cs=default_code_stylist) md =
  fst (html_and_headers_of_md ~pindent:pindent ~nl2br:nl2br ~cs:cs md)
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
    | Ref(rc, name, text, fallback) :: tl ->
        bprintf b "(Ref %S %S)" name text;
        loop tl
    | Img_ref(rc, name, alt, fallback) :: tl ->
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
    | Olp l :: tl ->
        bprintf b "(Olp";
        List.iter(fun li -> bprintf b "(Li "; loop li; bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Ulp l :: tl ->
        bprintf b "(Ulp";
        List.iter(fun li -> bprintf b "(Li "; loop li;bprintf b ")") l;
        bprintf b ")";
        loop tl
    | Code(lang, c) :: tl ->
        bprintf b "(Code %S)" c;
        loop tl
    | Code_block(lang, c) :: tl ->
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
    | Html_comment s :: tl ->
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


let rec markdown_of_md md =
  let quote s =
    let b = Buffer.create (String.length s) in
    let l = String.length s in
    let rec loop nl i =
      if i < l then
        begin
          if nl then Buffer.add_string b "> ";
          match s.[i] with
          | '\n' ->
            Buffer.add_char b '\n';
            loop true (succ i)
          | c ->
            Buffer.add_char b c;
            loop false (succ i)
        end
      else
        Buffer.contents b
    in loop true 0
  in
  let b = Buffer.create 42 in
  let add_spaces n = for i = 1 to n do Buffer.add_char b ' ' done in
  let references = ref None in
  let rec loop list_indent = function
    | X x :: tl ->
        (match x#to_t md with
           | Some t -> loop list_indent t
           | None ->
             match x#to_html ~indent:0 html_of_md md with
             | Some s -> Buffer.add_string b s
             | None -> ());
        loop list_indent tl
    | Blockquote q :: tl ->
      Buffer.add_string b (quote(markdown_of_md q));
      loop list_indent tl
    | Ref(rc, name, text, fallback) :: tl ->
      references := Some rc;
      Printf.bprintf b "[%s][%s]" text name;
      loop list_indent tl
    | Img_ref(rc, name, alt, fallback) :: tl ->
      references := Some rc;
      Printf.bprintf b "![%s][%s]" name alt;
      loop list_indent tl
    | Paragraph md :: tl ->
      loop list_indent md;
      Printf.bprintf b "\n\n";
      loop list_indent tl
    | Img(alt, src, title) :: tl ->
      Printf.bprintf b "![%s](%s \"%s\")" alt src title;
      loop list_indent tl
    | Text t :: tl ->
      Printf.bprintf b "%s" t;
      loop list_indent tl
    | Emph md :: tl ->
      Buffer.add_string b "*";
      loop list_indent md;
      Buffer.add_string b "*";
      loop list_indent tl
    | Bold md :: tl ->
      Buffer.add_string b "**";
      loop list_indent md;
      Buffer.add_string b "**";
      loop list_indent tl
    | Ol l :: tl ->
      List.iter(fun li -> add_spaces list_indent; Printf.bprintf b "1. "; loop (list_indent+4) li) l;
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | Ul l :: tl ->
      List.iter(fun li -> add_spaces list_indent; Printf.bprintf b "* "; loop (list_indent+4) li) l;
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | Olp l :: tl ->
      List.iter(fun li -> add_spaces list_indent; Printf.bprintf b "1. "; loop (list_indent+4) li) l;
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | Ulp l :: tl ->
      List.iter(fun li -> add_spaces list_indent; Printf.bprintf b "- "; loop (list_indent+4) li) l;
      if list_indent = 0 then Buffer.add_char b '\n';
      loop list_indent tl
    | Code(lang, c) :: tl -> (* FIXME *)
      let n = (* compute how many backquotes we need to use *)
        let filter (n:int) (s:int list) =
          if n > 0 && n < 10 then
            List.filter (fun e -> e <> n) s
          else
            s
        in
        let l = String.length c in
        let rec loop s x b i =
          if i = l then
            match filter b s with
            | hd::_ -> hd
            | [] -> x+1
          else
            match c.[i] with
            | '`' -> loop s x (succ b) (succ i)
            | _ -> loop (filter b s) (max b x) 0 (succ i)
        in
          loop [1;2;3;4;5;6;7;8;9;10] 0 0 0
      in
        begin
          Printf.bprintf b "%s" (String.make n '`');
          if c.[0] = '`' then Buffer.add_char b ' ';
          Printf.bprintf b "%s" c;
          if c.[String.length c - 1] = '`' then Buffer.add_char b ' ';
          Printf.bprintf b "%s" (String.make n '`');
        end;
        loop list_indent tl
    | Code_block(lang, c) :: tl ->
      let n = (* compute how many backquotes we need to use *)
        let filter n s =
          if n > 0 && n < 10 then
            List.filter (fun e -> e <> n) s
          else
            s
        in
        let l = String.length c in
        let rec loop s b i =
          if i = l then
            match filter b s with
              | hd::_ -> hd
              | [] -> 0
          else
            match c.[i] with
            | '`' -> loop s (succ b) (succ i)
            | _ -> loop (filter b s) 0 (succ i)
        in
          loop [3;4;5;6;7;8;9;10] 0 0
      in
        if n = 0 then  (* FIXME *)
          (* case where we can't use backquotes *)
          let output_indented_block n s =
            let rec loop p i =
              if i = String.length s then
                ()
              else
                match p with
                | '\n' ->
                    Printf.bprintf b "%s" (String.make n ' ');
                    Buffer.add_char b s.[i];
                    loop s.[i] (succ i)
                | _ ->
                    Buffer.add_char b s.[i];
                    loop s.[i] (succ i)
            in loop '\n' 0
          in
          Buffer.add_char b '\n';
          output_indented_block (4+list_indent) c;
          Buffer.add_string b "\n\n"
        else
          begin
            Printf.bprintf b "%s%s\n" (String.make n '`')
              (if lang = "" then !default_language else lang);
            Printf.bprintf b "%s" c;
            Printf.bprintf b "%s\n" (String.make n '`');
          end;
        loop list_indent tl
    | Br :: tl ->
      Buffer.add_string b "<br />";
      loop list_indent tl
    | Hr :: tl ->
      Buffer.add_string b "* * *";
      loop list_indent tl
    | Html s :: tl ->
      Buffer.add_string b s;
      loop list_indent tl
    | Html_block s :: tl ->
      Buffer.add_string b "\n\n";
      Buffer.add_string b s;
      Buffer.add_string b "\n\n";
      loop list_indent tl
    | Html_comment s :: tl ->
      Buffer.add_string b s;
      loop list_indent tl
    | Url (href,s,title) :: tl ->
      if title = "" then
        bprintf b "[%s](%s)" (html_of_md s) href
      else
        bprintf b "[%s](%s \"%s\")" (html_of_md s) href title;
      loop list_indent tl
    | H1 md :: tl ->
      Buffer.add_string b "# ";
      loop list_indent md;
      Buffer.add_string b "";
      loop list_indent tl
    | H2 md :: tl ->
      Buffer.add_string b "## ";
      loop list_indent md;
      Buffer.add_string b "";
      loop list_indent tl
    | H3 md :: tl ->
      Buffer.add_string b "### ";
      loop list_indent md;
      Buffer.add_string b "";
      loop list_indent tl
    | H4 md :: tl ->
      Buffer.add_string b "#### ";
      loop list_indent md;
      Buffer.add_string b "";
      loop list_indent tl
    | H5 md :: tl ->
      Buffer.add_string b "##### ";
      loop list_indent md;
      Buffer.add_string b "";
      loop list_indent tl
    | H6 md :: tl ->
      Buffer.add_string b "###### ";
      loop list_indent md;
      Buffer.add_string b "";
      loop list_indent tl
    | NL :: tl ->
        Buffer.add_string b "\n";
        loop list_indent tl
    | [] -> ()
  in
    loop 0 md;
    begin match !references with
      | None -> ()
      | Some r ->
          Buffer.add_char b '\n';
          List.iter
            (fun (name, (url, title)) ->
               if title = "" then
                 bprintf b "[%s]: %s \n" name url
               else
                 bprintf b "[%s]: %s \"%s\"\n" name url title
            )
            r#get_all                    
    end;
    Buffer.contents b

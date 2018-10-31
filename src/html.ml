open Ast

let percent_encode s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '!' | '*' | '\'' | '(' | ')' | ';' | ':'
      | '@' | '&' | '=' | '+' | '$' | ',' | '/' | '?'
      | '#' | '[' | ']'
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' as c ->
          Buffer.add_char b c
      | _ as c ->
          Printf.bprintf b "%%%2X" (Char.code c)
    ) s;
  Buffer.contents b

let rec html_of_md b md =
  let rec loop = function
    | Concat l ->
        List.iter loop l
    | Img_ref _ -> assert false
    | Img (alt, src, title) ->
        Buffer.add_string b "<img src=\"";
        Buffer.add_string b (Utils.htmlentities ~md:true src);
        Buffer.add_string b "\" alt=\"";
        Buffer.add_string b (Utils.htmlentities ~md:true alt);
        Buffer.add_string b "\" ";
        if title <> "" then begin
          Buffer.add_string b " title='";
          Buffer.add_string b (Utils.htmlentities ~md:true title);
          Buffer.add_string b "' "
        end;
        Buffer.add_string b "/>"
    | Text t ->
        (* Buffer.add_string b t; *)
        Buffer.add_string b (Utils.htmlentities ~md:true t)
    | Emph (Normal, _, md) ->
        Buffer.add_string b "<em>";
        loop md;
        Buffer.add_string b "</em>"
    | Emph (Strong, _, md) ->
        Buffer.add_string b "<strong>";
        loop md;
        Buffer.add_string b "</strong>"
    | Code c ->
        Buffer.add_string b "<code>";
        Buffer.add_string b (Utils.htmlentities ~md:false c);
        Buffer.add_string b "</code>"
    | Hard_break ->
        Buffer.add_string b "<br />\n"
    | Html body ->
        Buffer.add_string b body
    | Url {label; destination; title} | Url_ref (label, {destination; title; _}) ->
        Buffer.add_string b "<a href=\"";
        Buffer.add_string b (percent_encode destination);
        Buffer.add_string b "\"";
        begin match title with
        | None -> ()
        | Some title ->
            Buffer.add_string b " title=\"";
            Buffer.add_string b (Utils.htmlentities ~md:true title);
            Buffer.add_string b "\""
        end;
        Buffer.add_string b ">";
        html_of_md b label;
        Buffer.add_string b "</a>"
    | Soft_break ->
        Buffer.add_string b "\n"
  in
  loop md

let to_html b md =
  let iter_par f l = List.iter (function Link_def _ -> () | md -> f md) l in
  let rec loop = function
    | Blockquote q ->
        Buffer.add_string b "<blockquote>\n";
        iter_par (fun md -> loop md; Buffer.add_char b '\n') q;
        Buffer.add_string b "</blockquote>"
    | Paragraph md ->
        Buffer.add_string b "<p>";
        html_of_md b md;
        Buffer.add_string b "</p>"
    | List (kind, style, l) ->
        Buffer.add_string b
          (match kind with
           | Ordered (1, _) -> "<ol>\n"
           | Ordered (n, _) -> "<ol start=\"" ^ string_of_int n ^ "\">\n"
           | Unordered _ -> "<ul>\n");
        List.iter (fun x ->
            Buffer.add_string b "<li>";
            let _ = List.fold_left (li style) false x in
            Buffer.add_string b "</li>\n"
          ) l;
        Buffer.add_string b
          (match kind with Ordered _ -> "</ol>" | Unordered _ -> "</ul>")
    | Code_block ((None | Some (_, "")), None) ->
        Buffer.add_string b "<pre><code></code></pre>"
    | Code_block (Some (_, info), None) ->
        Buffer.add_string b (Printf.sprintf "<pre><code class=\"language-%s\"></code></pre>" info)
    | Code_block ((None | Some (_, "")), Some c) ->
        Buffer.add_string b "<pre><code>";
        Buffer.add_string b (Utils.htmlentities ~md:false c);
        Buffer.add_string b "\n</code></pre>"
    | Code_block (Some (_, info), Some c) ->
        Printf.bprintf b "<pre><code class=\"language-%s\">" info;
        Buffer.add_string b (Utils.htmlentities ~md:false c);
        Buffer.add_string b "\n</code></pre>"
    | Thematic_break ->
        Buffer.add_string b "<hr />"
    | Html_block body ->
        Buffer.add_string b body
    | Heading (i, md) ->
        Buffer.add_string b (Printf.sprintf "<h%d>" i);
        html_of_md b md;
        Buffer.add_string b (Printf.sprintf "</h%d>" i)
    | Link_def _ ->
        ()
  and li style prev_nl x =
    match x, style with
    | Paragraph md, Tight ->
        html_of_md b md;
        false
    | Link_def _, _ ->
        prev_nl
    | _ ->
        if not prev_nl then Buffer.add_char b '\n';
        loop x;
        Buffer.add_char b '\n';
        true
  in
  loop md

let to_html mds =
  let b = Buffer.create 64 in
  List.iter (function
    | Link_def _ -> ()
    | md ->
        to_html b md;
        Buffer.add_char b '\n'
    ) mds;
  Buffer.contents b

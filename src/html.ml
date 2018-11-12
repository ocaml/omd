open Ast

let _id_of_string ids s =
  let n = String.length s in
  let out = Buffer.create 0 in
  (* Put [s] into [b], replacing non-alphanumeric characters with dashes. *)
  let rec loop started i =
    if i = n then ()
    else begin
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c ->
          Buffer.add_char out c ;
          loop true (i + 1)
      (* Don't want to start with dashes. *)
      | _ when not started ->
          loop false (i + 1)
      | _ ->
          Buffer.add_char out '-' ;
          loop false (i + 1)
    end
  in
  loop false 0 ;
  let s' = Buffer.contents out in
  if s' = "" then ""
  else
    (* Find out the index of the last character in [s'] that isn't a dash. *)
    let last_trailing =
      let rec loop i =
        if i < 0 || s'.[i] <> '-' then i
        else loop (i - 1)
      in
      loop (String.length s' - 1)
    in
    (* Trim trailing dashes. *)
    ids#mangle @@ String.sub s' 0 (last_trailing + 1)

(* only convert when "necessary" *)
let htmlentities s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then
      Buffer.contents b
    else begin
      begin match s.[i] with
      | '"' ->
          Buffer.add_string b "&quot;"
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | c ->
          Buffer.add_char b c
      end;
      loop (succ i)
    end
  in
  loop 0

let percent_encode s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '!' | '*' | '\'' | '(' | ')' | ';' | ':'
      | '@' | '=' | '+' | '$' | ',' | '/' | '?' | '%'
      | '#' | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' as c ->
          Buffer.add_char b c
      | '&' ->
          Buffer.add_string b "&amp;"
      | _ as c ->
          Printf.bprintf b "%%%2X" (Char.code c)
    ) s;
  Buffer.contents b

let text_of_inline ast =
  let b = Buffer.create 101 in
  Text.inline b ast;
  Buffer.contents b

let rec html_of_md b md =
  let rec loop = function
    | Concat l ->
        List.iter loop l
    | Text t ->
        Buffer.add_string b (htmlentities t)
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
        Buffer.add_string b (htmlentities c);
        Buffer.add_string b "</code>"
    | Hard_break ->
        Buffer.add_string b "<br />\n"
    | Html body ->
        Buffer.add_string b body
    | Link (Url, {label; destination; title}) | Ref (Url, label, {destination; title; _}) ->
        Buffer.add_string b "<a href=\"";
        Buffer.add_string b (percent_encode destination);
        Buffer.add_string b "\"";
        begin match title with
        | None -> ()
        | Some title ->
            Buffer.add_string b " title=\"";
            Buffer.add_string b (htmlentities title);
            Buffer.add_string b "\""
        end;
        Buffer.add_string b ">";
        html_of_md b label;
        Buffer.add_string b "</a>"
    | Link (Img, {label; destination; title}) | Ref (Img, label, {destination; title; _}) ->
        Buffer.add_string b "<img src=\"";
        Buffer.add_string b (percent_encode destination);
        Buffer.add_string b "\" alt=\"";
        Buffer.add_string b (htmlentities (text_of_inline label));
        Buffer.add_string b "\"";
        begin match title with
        | None -> ()
        | Some title ->
            Buffer.add_string b " title=\"";
            Buffer.add_string b (htmlentities title);
            Buffer.add_string b "\""
        end;
        Buffer.add_string b " />"
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
        Buffer.add_string b (htmlentities c);
        Buffer.add_string b "\n</code></pre>"
    | Code_block (Some (_, info), Some c) ->
        Printf.bprintf b "<pre><code class=\"language-%s\">" info;
        Buffer.add_string b (htmlentities c);
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

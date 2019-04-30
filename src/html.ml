open Ast

type printer =
  {
    start: printer          -> Buffer.t -> inline block list                                             -> unit;
    finish: printer         -> Buffer.t                                                                  -> unit;
    block: printer          -> Buffer.t -> inline block                                                  -> unit;
    paragraph: printer      -> Buffer.t -> inline                                                        -> unit;
    blockquote: printer     -> Buffer.t -> inline block list                                             -> unit;
    list: printer           -> Buffer.t -> list_kind * list_style * inline block list list               -> unit;
    code_block: printer     -> Buffer.t -> (fenced_code_kind * (string * string)) option * string option -> unit;
    thematic_break: printer -> Buffer.t                                                                  -> unit;
    html_block: printer     -> Buffer.t -> string                                                        -> unit;
    heading: printer        -> Buffer.t -> int * inline                                                  -> unit;
    inline: printer         -> Buffer.t -> inline                                                        -> unit;
    concat: printer         -> Buffer.t -> inline list                                                   -> unit;
    text: printer           -> Buffer.t -> string                                                        -> unit;
    emph: printer           -> Buffer.t -> emph_kind * emph_style * inline                               -> unit;
    code: printer           -> Buffer.t -> int * string                                                  -> unit;
    hard_break: printer     -> Buffer.t                                                                  -> unit;
    soft_break: printer     -> Buffer.t                                                                  -> unit;
    html: printer           -> Buffer.t -> string                                                        -> unit;
    link: printer           -> Buffer.t -> link_kind * inline link_def                                   -> unit;
    ref: printer            -> Buffer.t -> link_kind * inline * string link_def                          -> unit;
  }

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

let start_print p b mds =
  List.iter (function
    | Link_def _ -> ()
    | md ->
        p.block p b md;
        Buffer.add_char b '\n'
    ) mds;
  p.finish p b

let finish_print _ _ = ()

let print_concat p b l =
  List.iter (p.inline p b) l

let print_text _ b t =
  Buffer.add_string b (htmlentities t)

let print_emph p b = function
  | Normal, _, md ->
      Buffer.add_string b "<em>";
      p.inline p b md;
      Buffer.add_string b "</em>"
  | Strong, _, md ->
      Buffer.add_string b "<strong>";
      p.inline p b md;
      Buffer.add_string b "</strong>"

let print_code _ b (_, c) =
  Buffer.add_string b "<code>";
  Buffer.add_string b (htmlentities c);
  Buffer.add_string b "</code>"

let print_hard_break _ b =
  Buffer.add_string b "<br />\n"

let print_soft_break _ b =
  Buffer.add_string b "\n"

let print_html _ b body =
  Buffer.add_string b body

let print_url p b label destination title =
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
  p.inline p b label;
  Buffer.add_string b "</a>"

let print_img b label destination title =
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

let print_link p b = function
  | Url, {label; destination; title} ->
    print_url p b label destination title
  | Img, {label; destination; title}  ->
    print_img b label destination title

let print_ref p b = function
  | Url, label, {destination; title; _} ->
    print_url p b label destination title
  | Img, label, {destination; title; _} ->
    print_img b label destination title

let print_inline p b = function
  | Concat l ->
    p.concat p b l
  | Text t ->
    (* Format.eprintf "Text: %s\n" (id_of_string t); *)
    p.text p b t
  | Emph (kind, style, md) ->
    p.emph p b (kind, style, md)
  | Code (i, c) ->
    p.code p b (i, c)
  | Hard_break ->
    p.hard_break p b
  | Soft_break ->
    p.soft_break p b
  | Html body ->
    p.html p b body
  | Link (kind, link) ->
    p.link p b (kind, link)
  | Ref (kind, md, link) ->
    p.ref p b (kind, md, link)

let print_blockquote p b q =
  let iter_par f l = List.iter (function Link_def _ -> () | md -> f md) l in
  Buffer.add_string b "<blockquote>\n";
  iter_par (fun md -> p.block p b md; Buffer.add_char b '\n') q;
  Buffer.add_string b "</blockquote>"

let print_paragraph p b md =
  Buffer.add_string b "<p>";
  p.inline p b md;
  Buffer.add_string b "</p>"

let print_list p b (kind, style, l) =
  Buffer.add_string b
    (match kind with
      | Ordered (1, _) -> "<ol>\n"
      | Ordered (n, _) -> "<ol start=\"" ^ string_of_int n ^ "\">\n"
      | Unordered _ -> "<ul>\n");
  let li style prev_nl x =
    match x, style with
    | Paragraph md, Tight ->
        p.inline p b md;
        false
    | Link_def _, _ ->
        prev_nl
    | _ ->
        if not prev_nl then Buffer.add_char b '\n';
        p.block p b x;
        Buffer.add_char b '\n';
        true in
  List.iter (fun x ->
      Buffer.add_string b "<li>";
      let _ = List.fold_left (li style) false x in
      Buffer.add_string b "</li>\n"
    ) l;
  Buffer.add_string b
    (match kind with Ordered _ -> "</ol>" | Unordered _ -> "</ul>")

let print_code_block _ b = function
  | (None | Some (_, ("", _))), None ->
      Buffer.add_string b "<pre><code></code></pre>"
  | Some (_, (info, _)), None ->
      Buffer.add_string b (Printf.sprintf "<pre><code class=\"language-%s\"></code></pre>" info)
  | (None | Some (_, ("", _))), Some c ->
      Buffer.add_string b "<pre><code>";
      Buffer.add_string b (htmlentities c);
      Buffer.add_string b "\n</code></pre>"
  | Some (_, (info, _)), Some c ->
      Printf.bprintf b "<pre><code class=\"language-%s\">" info;
      Buffer.add_string b (htmlentities c);
      Buffer.add_string b "\n</code></pre>"

let print_thematic_break _ b =
  Buffer.add_string b "<hr />"

let print_html_block _ b body =
  Buffer.add_string b body

let print_heading p b (i, md) =
  Buffer.add_string b (Printf.sprintf "<h%d>" i);
  p.inline p b md;
  Buffer.add_string b (Printf.sprintf "</h%d>" i)

let print_block p b = function
  | Blockquote q ->
    p.blockquote p b q
  | Paragraph md ->
    p.paragraph p b md
  | List (kind, style, l) ->
    p.list p b (kind, style, l)
  | Code_block (i, t) ->
    p.code_block p b (i, t)
  | Thematic_break ->
    p.thematic_break p b
  | Html_block body ->
    p.html_block p b body
  | Heading (i, md) ->
    p.heading p b (i, md)
  | Link_def _ ->
    ()

let default_printer =
  {
    start = start_print;
    finish = finish_print;
    block = print_block;
    paragraph = print_paragraph;
    blockquote = print_blockquote;
    list = print_list;
    code_block = print_code_block;
    thematic_break = print_thematic_break;
    html_block = print_html_block;
    heading = print_heading;
    inline = print_inline;
    concat = print_concat;
    text = print_text;
    emph = print_emph;
    code = print_code;
    hard_break = print_hard_break;
    soft_break = print_soft_break;
    html = print_html;
    link = print_link;
    ref = print_ref;
  }

let to_html ?(printer=default_printer) mds =
  let b = Buffer.create 64 in
  printer.start printer b mds;
  Buffer.contents b

open Ast

type printer =
  {
    document: printer       -> Buffer.t -> inline block list         -> unit;
    block: printer          -> Buffer.t -> inline block              -> unit;
    paragraph: printer      -> Buffer.t -> inline                    -> unit;
    blockquote: printer     -> Buffer.t -> inline block list         -> unit;
    list: printer           -> Buffer.t -> inline block Block_list.t -> unit;
    code_block: printer     -> Buffer.t -> Code_block.t              -> unit;
    thematic_break: printer -> Buffer.t                              -> unit;
    html_block: printer     -> Buffer.t -> string                    -> unit;
    heading: printer        -> Buffer.t -> inline Heading.t          -> unit;
    inline: printer         -> Buffer.t -> inline                    -> unit;
    concat: printer         -> Buffer.t -> inline list               -> unit;
    text: printer           -> Buffer.t -> string                    -> unit;
    emph: printer           -> Buffer.t -> inline Emph.t             -> unit;
    code: printer           -> Buffer.t -> int -> string             -> unit;
    hard_break: printer     -> Buffer.t                              -> unit;
    soft_break: printer     -> Buffer.t                              -> unit;
    html: printer           -> Buffer.t -> string                    -> unit;
    link: printer           -> Buffer.t -> inline Link.t             -> unit;
    ref: printer            -> Buffer.t -> inline Ref.t              -> unit;
  }

let id_of_string s =
  let n = Bytes.length s in
  let id = Buffer.create 64 in
  let rec trim i =
    if i = n then n
    else begin
      match Bytes.get s i with
      | 'a' .. 'z' | 'A' .. 'Z' -> i
      | _ -> trim (i + 1)
    end
  and loop keep i =
    if i = n then begin if not keep then Buffer.truncate id (Buffer.length id - 1) end
    else begin
      match Bytes.get s i with
      | 'A' .. 'Z' as c ->
          Buffer.add_char id (Char.lowercase_ascii c) ;
          loop true (i + 1)
      | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' as c ->
          Buffer.add_char id c ;
          loop true (i + 1)
      | ' ' | '\n' ->
          if keep then
          Buffer.add_char id '-' ;
          loop false (i + 1)
      | _ ->
          loop keep (i + 1)
      end
    in
    loop true (trim 0); Bytes.to_string (Buffer.to_bytes id)

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

let print_document p b mds =
  List.iter (function
      | Link_def _ -> ()
      | md ->
          p.block p b md;
          Buffer.add_char b '\n'
    ) mds

let print_concat p b l =
  List.iter (p.inline p b) l

let print_text _ b t =
  Buffer.add_string b (htmlentities t)

let print_emph p b (e: inline Emph.t) =
  match e.kind with
  | Normal ->
      Buffer.add_string b "<em>";
      p.inline p b e.content;
      Buffer.add_string b "</em>"
  | Strong ->
      Buffer.add_string b "<strong>";
      p.inline p b e.content;
      Buffer.add_string b "</strong>"

let print_code _ b _ c =
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

let print_link p b (l: inline Link.t) =
  match l.kind with
  | Url ->
      print_url p b l.def.label l.def.destination l.def.title
  | Img ->
      print_img b l.def.label l.def.destination l.def.title

let print_ref p b (r: inline Ref.t) =
  match r.kind with
  | Url ->
      print_url p b r.label r.def.destination r.def.title
  | Img ->
      print_img b r.label r.def.destination r.def.title

let print_inline p b = function
  | Concat l ->
      p.concat p b l
  | Text t ->
      p.text p b t
  | Emph e ->
      p.emph p b e
  | Code (i, c) ->
      p.code p b i c
  | Hard_break ->
      p.hard_break p b
  | Soft_break ->
      p.soft_break p b
  | Html body ->
      p.html p b body
  | Link l ->
      p.link p b l
  | Ref r ->
      p.ref p b r

let print_blockquote p b q =
  let iter_par f l = List.iter (function Link_def _ -> () | md -> f md) l in
  Buffer.add_string b "<blockquote>\n";
  iter_par (fun md -> p.block p b md; Buffer.add_char b '\n') q;
  Buffer.add_string b "</blockquote>"

let print_paragraph p b md =
  Buffer.add_string b "<p>";
  p.inline p b md;
  Buffer.add_string b "</p>"

let print_list p b (l: inline block Block_list.t) =
  Buffer.add_string b
    (match l.kind with
     | Ordered (1, _) -> "<ol>\n"
     | Ordered (n, _) -> "<ol start=\"" ^ string_of_int n ^ "\">\n"
     | Unordered _ -> "<ul>\n");
  let li style prev_nl x =
    match x, style with
    | Paragraph md, Block_list.Tight ->
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
      let _ = List.fold_left (li l.style) false x in
      Buffer.add_string b "</li>\n"
    ) l.blocks;
  Buffer.add_string b
    (match l.kind with Ordered _ -> "</ol>" | Unordered _ -> "</ul>")

let print_code_block _ b (c: Code_block.t) =
  match c with
  | {label = None | Some ""; code = None; _} ->
      Buffer.add_string b "<pre><code></code></pre>"
  | {label = Some language; code = None; _} ->
      Buffer.add_string b (Printf.sprintf "<pre><code class=\"language-%s\"></code></pre>" language)
  | {label = None | Some ""; code = Some c; _} ->
      Buffer.add_string b "<pre><code>";
      Buffer.add_string b (htmlentities c);
      Buffer.add_string b "\n</code></pre>"
  | {label = Some language; code = Some c; _} ->
      Printf.bprintf b "<pre><code class=\"language-%s\">" language;
      Buffer.add_string b (htmlentities c);
      Buffer.add_string b "\n</code></pre>"

let print_thematic_break _ b =
  Buffer.add_string b "<hr />"

let print_html_block _ b body =
  Buffer.add_string b body

let print_heading p b (h: inline Heading.t) =
  Buffer.add_string b (Printf.sprintf "<h%d" h.level);
  let id =
    match h.attributes.id with
    | None ->
      let b' = Buffer.create 64 in
      p.inline p b' h.text;
      id_of_string (Buffer.to_bytes b')
    | Some s -> s
  in
  Buffer.add_string b (Printf.sprintf " id=\"%s\"" id);
  if List.length h.attributes.classes <> 0
  then
    begin
      Buffer.add_string b (Printf.sprintf " class=\"%s\"" (String.concat " " h.attributes.classes))
    end;
  if List.length h.attributes.attributes <> 0
  then
    begin
      let f (d, v) = Buffer.add_string b (Printf.sprintf " data-%s=\"%s\"" d v) in
      List.iter f h.attributes.attributes
    end;
  Buffer.add_string b (Printf.sprintf ">");
  p.inline p b h.text;
  Buffer.add_string b (Printf.sprintf "</h%d>" h.level)

let print_block p b = function
  | Blockquote q ->
      p.blockquote p b q
  | Paragraph md ->
      p.paragraph p b md
  | List l ->
      p.list p b l
  | Code_block c ->
      p.code_block p b c
  | Thematic_break ->
      p.thematic_break p b
  | Html_block body ->
      p.html_block p b body
  | Heading h ->
      p.heading p b h
  | Link_def _ ->
      ()

let default_printer =
  {
    document = print_document;
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
  printer.document printer b mds;
  Buffer.contents b

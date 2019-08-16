open Ast

type printer =
  {
    document: printer       -> Buffer.t -> inline block list         -> unit;
    attributes: printer     -> Buffer.t -> Attributes.t              -> unit;
    block: printer          -> Buffer.t -> inline block              -> unit;
    paragraph: printer      -> Buffer.t -> inline                    -> unit;
    blockquote: printer     -> Buffer.t -> inline block list         -> unit;
    list: printer           -> Buffer.t -> inline block Block_list.t -> unit;
    code_block: printer     -> Buffer.t -> Code_block.t              -> unit;
    thematic_break: printer -> Buffer.t                              -> unit;
    html_block: printer     -> Buffer.t -> string                    -> unit;
    heading: printer        -> Buffer.t -> inline Heading.t          -> unit;
    tag_block: printer      -> Buffer.t -> inline block Tag_block.t  -> unit;
    inline: printer         -> Buffer.t -> inline                    -> unit;
    concat: printer         -> Buffer.t -> inline list               -> unit;
    text: printer           -> Buffer.t -> string                    -> unit;
    emph: printer           -> Buffer.t -> inline Emph.t             -> unit;
    code: printer           -> Buffer.t -> Code.t                    -> unit;
    hard_break: printer     -> Buffer.t                              -> unit;
    soft_break: printer     -> Buffer.t                              -> unit;
    html: printer           -> Buffer.t -> string                    -> unit;
    link: printer           -> Buffer.t -> inline Link.t             -> unit;
    ref: printer            -> Buffer.t -> inline Ref.t              -> unit;
    tag: printer            -> Buffer.t -> inline Tag.t              -> unit;
  }

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

let print_attributes _ b attr =
  begin
    match attr.Attributes.id with
    | None -> ()
    | Some s -> Buffer.add_string b (Printf.sprintf " id=\"%s\"" s)
  end;
  if List.length attr.classes <> 0
  then
    begin
      Buffer.add_string b (Printf.sprintf " class=\"%s\"" (String.concat " " attr.classes))
    end;
  if List.length attr.attributes <> 0
  then
    begin
      let f (d, v) = Buffer.add_string b (Printf.sprintf " %s=\"%s\"" d v) in
      List.iter f attr.attributes
    end

let print_concat p b l =
  List.iter (p.inline p b) l

let print_text _ b t =
  Buffer.add_string b (htmlentities t)

let print_emph p b e =
  match e.Emph.kind with
  | Normal ->
      Buffer.add_string b "<em>";
      p.inline p b e.content;
      Buffer.add_string b "</em>"
  | Strong ->
      Buffer.add_string b "<strong>";
      p.inline p b e.content;
      Buffer.add_string b "</strong>"

let print_code p b c =
  Buffer.add_string b "<code";
  p.attributes p b c.Code.attributes;
  Buffer.add_string b ">";
  Buffer.add_string b (htmlentities c.content);
  Buffer.add_string b "</code>"

let print_hard_break _ b =
  Buffer.add_string b "<br />\n"

let print_soft_break _ b =
  Buffer.add_string b "\n"

let print_html _ b body =
  Buffer.add_string b body

let print_url p b label destination title attributes =
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
  p.attributes p b attributes;
  Buffer.add_string b ">";
  p.inline p b label;
  Buffer.add_string b "</a>"

let print_img p b label destination title attributes =
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
  p.attributes p b attributes;
  Buffer.add_string b " />"

let print_link p b l =
  match l.Link.kind with
  | Url ->
      print_url p b l.def.label l.def.destination l.def.title l.def.attributes
  | Img ->
      print_img p b l.def.label l.def.destination l.def.title l.def.attributes

let print_ref p b r =
  match r.Ref.kind with
  | Url ->
      print_url p b r.label r.def.destination r.def.title r.def.attributes
  | Img ->
      print_img p b r.label r.def.destination r.def.title r.def.attributes

let print_tag p b t =
  p.inline p b t.Tag.content

let print_inline p b = function
  | Concat l ->
      p.concat p b l
  | Text t ->
      p.text p b t
  | Emph e ->
      p.emph p b e
  | Code c ->
      p.code p b c
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
  | Tag t ->
      p.tag p b t

let print_blockquote p b q =
  let iter_par f l = List.iter (function Link_def _ -> () | md -> f md) l in
  Buffer.add_string b "<blockquote>\n";
  iter_par (fun md -> p.block p b md; Buffer.add_char b '\n') q;
  Buffer.add_string b "</blockquote>"

let print_paragraph p b md =
  Buffer.add_string b "<p>";
  p.inline p b md;
  Buffer.add_string b "</p>"

let print_list p b l =
  Buffer.add_string b
    (match l.Block_list.kind with
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

let print_code_block p b c =
  Buffer.add_string b "<pre";
  p.attributes p b c.Code_block.attributes;
  match c with
  | {label = None | Some ""; code = None; _} ->
      Buffer.add_string b "><code></code></pre>"
  | {label = Some language; code = None; _} ->
      Buffer.add_string b (Printf.sprintf "><code class=\"language-%s\"></code></pre>" language)
  | {label = None | Some ""; code = Some c; _} ->
      Buffer.add_string b "><code>";
      Buffer.add_string b (htmlentities c);
      Buffer.add_string b "\n</code></pre>"
  | {label = Some language; code = Some c; _} ->
      Printf.bprintf b "><code class=\"language-%s\">" language;
      Buffer.add_string b (htmlentities c);
      Buffer.add_string b "\n</code></pre>"

let print_thematic_break _ b =
  Buffer.add_string b "<hr />"

let print_html_block _ b body =
  Buffer.add_string b body

let print_heading p b h =
  Buffer.add_string b (Printf.sprintf "<h%d" h.Heading.level);
  p.attributes p b h.attributes;
  Buffer.add_string b (Printf.sprintf ">");
  p.inline p b h.text;
  Buffer.add_string b (Printf.sprintf "</h%d>" h.level)

let print_tag_block p b t =
  let f i block =
    p.block p b block;
    if i < List.length t.Tag_block.content - 1 then
      Buffer.add_char b '\n'
  in
  List.iteri f t.Tag_block.content

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
  | Tag_block t ->
      p.tag_block p b t
  | Link_def _ ->
      ()

let default_printer =
  {
    document = print_document;
    attributes = print_attributes;
    block = print_block;
    paragraph = print_paragraph;
    blockquote = print_blockquote;
    list = print_list;
    code_block = print_code_block;
    thematic_break = print_thematic_break;
    html_block = print_html_block;
    heading = print_heading;
    tag_block = print_tag_block;
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
    tag = print_tag;
  }

let to_html ?(printer=default_printer) mds =
  let b = Buffer.create 64 in
  printer.document printer b mds;
  Buffer.contents b

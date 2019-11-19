open Ast
open Printer

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

let document p b mds =
  List.iter (function
      | Link_def _ -> ()
      | md ->
          p.block p b md;
          Buffer.add_char b '\n'
    ) mds

let attributes _ b attr =
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

let concat p b l =
  List.iter (p.inline p b) l

let text _ b t =
  Buffer.add_string b (htmlentities t)

let emph p b e =
  match e.Emph.kind with
  | Normal ->
      Buffer.add_string b "<em>";
      p.inline p b e.content;
      Buffer.add_string b "</em>"
  | Strong ->
      Buffer.add_string b "<strong>";
      p.inline p b e.content;
      Buffer.add_string b "</strong>"

let code p b c =
  Buffer.add_string b "<code";
  p.attributes p b c.Code.attributes;
  Buffer.add_string b ">";
  Buffer.add_string b (htmlentities c.content);
  Buffer.add_string b "</code>"

let hard_break _ b =
  Buffer.add_string b "<br />\n"

let soft_break _ b =
  Buffer.add_string b "\n"

let html _ b body =
  Buffer.add_string b body

let url p b label destination title attributes =
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

let img p b label destination title attributes =
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

let link p b l =
  match l.Link.kind with
  | Url ->
      url p b l.def.label l.def.destination l.def.title l.def.attributes
  | Img ->
      img p b l.def.label l.def.destination l.def.title l.def.attributes

let ref p b r =
  match r.Ref.kind with
  | Url ->
      url p b r.label r.def.destination r.def.title r.def.attributes
  | Img ->
      img p b r.label r.def.destination r.def.title r.def.attributes

let tag p b t =
  p.inline p b t.Tag.content

let inline p b = function
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

let blockquote p b q =
  let iter_par f l = List.iter (function Link_def _ -> () | md -> f md) l in
  Buffer.add_string b "<blockquote>\n";
  iter_par (fun md -> p.block p b md; Buffer.add_char b '\n') q;
  Buffer.add_string b "</blockquote>"

let paragraph p b md =
  Buffer.add_string b "<p>";
  p.inline p b md;
  Buffer.add_string b "</p>"

let list p b l =
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

let code_block p b c =
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

let thematic_break _ b =
  Buffer.add_string b "<hr />"

let html_block _ b body =
  Buffer.add_string b body

let heading p b h =
  Buffer.add_string b (Printf.sprintf "<h%d" h.Heading.level);
  p.attributes p b h.attributes;
  Buffer.add_string b (Printf.sprintf ">");
  p.inline p b h.text;
  Buffer.add_string b (Printf.sprintf "</h%d>" h.level)

let def_list p b l =
  Buffer.add_string b (Printf.sprintf "<dl>\n");
  List.iter (fun {Def_list.term; defs} ->
    Buffer.add_string b (Printf.sprintf "<dt>");
    p.inline p b term;
    Buffer.add_string b (Printf.sprintf "</dt>\n");
    List.iter (fun s ->
      Buffer.add_string b (Printf.sprintf "<dd>");
      p.inline p b s;
      Buffer.add_string b (Printf.sprintf "</dd>\n")
    ) defs;
  ) l.Def_list.content;
  Buffer.add_string b (Printf.sprintf "</dl>")

let tag_block p b t =
  let f i block =
    p.block p b block;
    if i < List.length t.Tag_block.content - 1 then
      Buffer.add_char b '\n'
  in
  List.iteri f t.Tag_block.content

let block p b = function
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
  | Def_list l ->
      p.def_list p b l
  | Tag_block t ->
      p.tag_block p b t
  | Link_def _ ->
      ()

let default_printer =
  {
    document;
    attributes;
    block;
    paragraph;
    blockquote;
    list;
    code_block;
    thematic_break;
    html_block;
    heading;
    def_list;
    tag_block;
    inline;
    concat;
    text;
    emph;
    code;
    hard_break;
    soft_break;
    html;
    link;
    ref;
    tag;
  }

let to_html ?(printer=default_printer) mds =
  let b = Buffer.create 64 in
  printer.document printer b mds;
  Buffer.contents b

open Ast

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

open Ast.Iterator

let document p add mds =
  List.iter (function
      | Link_def _ -> ()
      | md ->
          p.block p add md;
          add "\n"
    ) mds

let attributes _ add attr =
  begin
    match attr.Attributes.id with
    | None -> ()
    | Some s -> add (Printf.sprintf " id=\"%s\"" s)
  end;
  if List.length attr.classes <> 0
  then
    begin
      add (Printf.sprintf " class=\"%s\"" (String.concat " " attr.classes))
    end;
  if List.length attr.attributes <> 0
  then
    begin
      let f (d, v) = add (Printf.sprintf " %s=\"%s\"" d v) in
      List.iter f attr.attributes
    end

let text _ add t =
  add (htmlentities t)

let emph p add e =
  match e.Emph.kind with
  | Normal ->
      add "<em>";
      p.inline p add e.content;
      add "</em>"
  | Strong ->
      add "<strong>";
      p.inline p add e.content;
      add "</strong>"

let code p add c =
  add "<code";
  p.attributes p add c.Code.attributes;
  add ">";
  add (htmlentities c.content);
  add "</code>"

let hard_break _ add () =
  add "<br />\n"

let soft_break _ add () =
  add "\n"

let html _ add body =
  add body

let url p add label destination title attributes =
  add "<a href=\"";
  add (percent_encode destination);
  add "\"";
  begin match title with
  | None -> ()
  | Some title ->
      add " title=\"";
      add (htmlentities title);
      add "\""
  end;
  p.attributes p add attributes;
  add ">";
  p.inline p add label;
  add "</a>"

let img p add label destination title attributes =
  add "<img src=\"";
  add (percent_encode destination);
  add "\" alt=\"";
  add (htmlentities (text_of_inline label));
  add "\"";
  begin match title with
  | None -> ()
  | Some title ->
      add " title=\"";
      add (htmlentities title);
      add "\""
  end;
  p.attributes p add attributes;
  add " />"

let link p add l =
  match l.Link.kind with
  | Url ->
      url p add l.def.label l.def.destination l.def.title l.def.attributes
  | Img ->
      img p add l.def.label l.def.destination l.def.title l.def.attributes

let ref p add r =
  match r.Ref.kind with
  | Url ->
      url p add r.label r.def.destination r.def.title r.def.attributes
  | Img ->
      img p add r.label r.def.destination r.def.title r.def.attributes

let blockquote p add q =
  let iter_par f l = List.iter (function Link_def _ -> () | md -> f md) l in
  add "<blockquote>\n";
  iter_par (fun md -> p.block p add md; add "\n") q;
  add "</blockquote>"

let paragraph p add md =
  add "<p>";
  p.inline p add md;
  add "</p>"

let list p add l =
  add
    (match l.Block_list.kind with
     | Ordered (1, _) -> "<ol>\n"
     | Ordered (n, _) -> "<ol start=\"" ^ string_of_int n ^ "\">\n"
     | Unordered _ -> "<ul>\n");
  let li style prev_nl x =
    match x, style with
    | Paragraph md, Block_list.Tight ->
        p.inline p add md;
        false
    | Link_def _, _ ->
        prev_nl
    | _ ->
        if not prev_nl then add "\n";
        p.block p add x;
        add "\n";
        true in
  List.iter (fun x ->
      add "<li>";
      let _ = List.fold_left (li l.style) false x in
      add "</li>\n"
    ) l.blocks;
  add
    (match l.kind with Ordered _ -> "</ol>" | Unordered _ -> "</ul>")

let code_block p add c =
  add "<pre";
  p.attributes p add c.Code_block.attributes;
  match c with
  | {label = None | Some ""; code = None; _} ->
      add "><code></code></pre>"
  | {label = Some language; code = None; _} ->
      add (Printf.sprintf "><code class=\"language-%s\"></code></pre>" language)
  | {label = None | Some ""; code = Some c; _} ->
      add "><code>";
      add (htmlentities c);
      add "\n</code></pre>"
  | {label = Some language; code = Some c; _} ->
      add (Printf.sprintf "><code class=\"language-%s\">" language);
      add (htmlentities c);
      add "\n</code></pre>"

let thematic_break _ add () =
  add "<hr />"

let html_block _ add body =
  add body

let heading p add h =
  add (Printf.sprintf "<h%d" h.Heading.level);
  p.attributes p add h.attributes;
  add (Printf.sprintf ">");
  p.inline p add h.text;
  add (Printf.sprintf "</h%d>" h.level)

let def_list p add l =
  add (Printf.sprintf "<dl>\n");
  List.iter (fun {Def_list.term; defs} ->
      add (Printf.sprintf "<dt>");
      p.inline p add term;
      add (Printf.sprintf "</dt>\n");
      List.iter (fun s ->
          add (Printf.sprintf "<dd>");
          p.inline p add s;
          add (Printf.sprintf "</dd>\n")
        ) defs;
    ) l.Def_list.content;
  add (Printf.sprintf "</dl>")

let tag_block p add {Tag_block.tag; attributes=attr; content} =
  let f i block =
    p.block p add block;
    if i < List.length content - 1 then
      add "\n"
  in
  add "<"; add tag;
  attributes p add attr;
  add ">\n";
  List.iteri f content;
  add "\n</"; add tag; add ">"

let default_printer =
{ iter with
  document
; attributes
; paragraph
; blockquote
; list
; code_block
; thematic_break
; html_block
; heading
; def_list
; tag_block
; text
; emph
; code
; hard_break
; soft_break
; html
; link
; ref
}

let to_html ?(printer=default_printer) mds =
  let b = Buffer.create 64 in
  printer.document printer (Buffer.add_string b) mds;
  Buffer.contents b

open Ast
open Printer

let document p b mds =
  List.iter (function
      | Link_def _ -> ()
      | md ->
          p.block p b md;
          Buffer.add_char b '\n'
    ) mds

let hypertarget b a callback =
  match a.Attributes.id with
  | None -> callback ()
  | Some id ->
    Buffer.add_string b (Printf.sprintf "\\hypertarget{%s}{%%\n" id);
    callback ();
    Buffer.add_string b (Printf.sprintf "\\label{%s}}" id)

let attributes _ b attr =
  begin
    match attr.Attributes.id with
    | None -> ()
    | Some s -> Buffer.add_string b (Printf.sprintf " label=%s" s)
  end;
  if List.length attr.attributes <> 0
  then
    begin
      let f (d, v) = Buffer.add_string b (Printf.sprintf " %s={%s}" d v) in
      List.iter f attr.attributes
    end

let concat p b l =
  List.iter (p.inline p b) l

let text _ b t =
  Buffer.add_string b t

let emph p b e =
  match e.Emph.kind with
  | Normal ->
      Buffer.add_string b "\\emph{";
      p.inline p b e.content;
      Buffer.add_string b "}"
  | Strong ->
      Buffer.add_string b "\\textbf{";
      p.inline p b e.content;
      Buffer.add_string b "}"

let code p b c =
  Buffer.add_string b "\\lstinline[";
  p.attributes p b c.Code.attributes;
  Buffer.add_string b "]!";
  Buffer.add_string b (c.content);
  Buffer.add_string b "!"

let hard_break _ b =
  Buffer.add_string b "\\\\\n"

let soft_break _ b =
  Buffer.add_string b " "

let html _ _ _ =
  ()

let url p b label destination _title _attributes =
  Buffer.add_string b (Printf.sprintf "\\href{%s}{" destination);
  p.inline p b label;
  Buffer.add_string b "}"

let img p b label destination _title attributes =
  Buffer.add_string b "\\begin{figure}\n";
  let graphics () =
    Buffer.add_string b (Printf.sprintf "\\centering\n\\includegraphics{%s}\n\\caption{" destination);
    p.inline p b label;
    Buffer.add_string b "}"
  in
  hypertarget b attributes graphics;
  Buffer.add_string b "\n\\end{figure}"

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
  Buffer.add_string b "\\begin{quote}\n";
  iter_par (fun md -> p.block p b md; Buffer.add_char b '\n') q;
  Buffer.add_string b "\\end{quote}"

let paragraph p b md =
  p.inline p b md

let list p b l =
  Buffer.add_string b
    (match l.Block_list.kind with
     | Ordered (1, _) -> "\\begin{enumerate}\n\\def\\labelenumi{\\arabic{enumi}.}\n"
     | Ordered (n, _) -> "\\begin{enumerate}\n\\def\\labelenumi{\\arabic{enumi}.}\n\\setcounter{enumi}{" ^ string_of_int (n - 1) ^ "}\n"
     | Unordered _ -> "\\begin{itemize}\n");
  Buffer.add_string b (Printf.sprintf "\\tightlist\n");
  List.iter (fun item ->
    Buffer.add_string b (Printf.sprintf "\\item\n  ");
    List.iter (fun s ->
      p.block p b s;
      Buffer.add_string b (Printf.sprintf "\n")
    ) item;
  ) l.Block_list.blocks;
  Buffer.add_string b
    (match l.Block_list.kind with
     | Ordered _ -> "\\end{enumerate}"
     | Unordered _ -> "\\end{itemize}")

let code_block p b c =
  Buffer.add_string b "\\begin{lstlisting}[";
  p.attributes p b
    (match c.Code_block.label with
    | None | Some "" -> c.Code_block.attributes
    | Some language ->  {c.Code_block.attributes with attributes = ("language", language)::c.Code_block.attributes.attributes});
  Buffer.add_string b "]\n";
  Buffer.add_string b (
    match c.Code_block.code with
    | None | Some "" -> ""
    | Some code -> code);
  Buffer.add_string b "\n\\end{lstlisting}"

let thematic_break _ b =
  Buffer.add_string b "\\begin{center}\\hrulefill\\end{center}"

let html_block _ _ _ =
  ()

let heading p b h =
  let section () =
    Buffer.add_string b (Printf.sprintf "\\section{");
    p.inline p b h.Heading.text;
    Buffer.add_string b (Printf.sprintf "}");
  in
  hypertarget b h.attributes section

let def_list p b l =
  Buffer.add_string b (Printf.sprintf "\\begin{description}\n");
  Buffer.add_string b (Printf.sprintf "\\tightlist\n");
  List.iter (fun {Def_list.term; defs} ->
    Buffer.add_string b (Printf.sprintf "\\item[");
    p.inline p b term;
    Buffer.add_string b (Printf.sprintf "]\n");
    List.iter (fun s ->
      p.inline p b s;
      Buffer.add_string b (Printf.sprintf "\n")
    ) defs;
  ) l.Def_list.content;
  Buffer.add_string b (Printf.sprintf "\\end{description}")

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

let to_latex ?(printer=default_printer) mds =
  let b = Buffer.create 64 in
  printer.document printer b mds;
  Buffer.contents b

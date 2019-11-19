open Ast
open Printer

let document p b mds =
  List.iter (function
      | Link_def _ -> ()
      | md ->
          p.block p b md;
          Buffer.add_char b '\n'
    ) mds

let attributes _ _b _attr =
  () (* Add implementation *)

let block _ _ _ =
  () (* Add implementation *)

let paragraph _ _ _ =
  () (* Add implementation *)

let blockquote _ _ _ =
  () (* Add implementation *)

let list _ _ _ =
  () (* Add implementation *)

let code_block _ _ _ =
  () (* Add implementation *)

let thematic_break _ _ =
  () (* Add implementation *)

let html_block _ _ _ =
  () (* Add implementation *)

let heading _ _ _ =
  () (* Add implementation *)

let def_list _ _ _ =
  () (* Add implementation *)

let tag_block _ _ _ =
  () (* Add implementation *)

let inline _ _ _ =
  () (* Add implementation *)

let concat _ _ _ =
  () (* Add implementation *)

let text _ _ _ =
  () (* Add implementation *)

let emph _ _ _ =
  () (* Add implementation *)

let code _ _ _ =
  () (* Add implementation *)

let hard_break _ _ =
  () (* Add implementation *)

let soft_break _ _ =
  () (* Add implementation *)

let html _ _ _ =
  () (* Add implementation *)

let link _ _ _ =
  () (* Add implementation *)

let ref _ _ _ =
  () (* Add implementation *)

let tag _ _ _ =
  () (* Add implementation *)


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

(* The document model *)

include Ast.Impl

(* Helper functions for construction document AST *)

module Ctor = Ast_constructors.Impl

(* Table of contents *)

let headers = Toc.headers
let toc = Toc.toc

(* Conversion *)

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

let parse_inlines (md, defs) : doc =
  let defs =
    let f (def : attributes Parser.link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Ast_block.Mapper.map (parse_inline defs)) md

let escape_html_entities = Html.htmlentities
let of_channel ic : doc = parse_inlines (Block_parser.Pre.of_channel ic)
let of_string s = parse_inlines (Block_parser.Pre.of_string s)

let to_html ?auto_identifiers doc =
  Html.to_string (Html.of_doc ?auto_identifiers doc)

let to_sexp ast = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

module Print = Print

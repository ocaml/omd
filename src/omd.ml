(* The document model *)

include Ast.Impl

(* Helper functions for construction document AST *)

module Ctor = Ast_constructors.Impl

(* Table of contents *)

let headers = Toc.headers
let toc = Toc.toc

(* Conversion *)

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

let parse_inlines (md, defs) : Cst.Impl.parse_tree =
  let defs =
    let f (def : Cst.Impl.attributes Parser.link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Cst_block.Mapper.map (parse_inline defs)) md

let escape_html_entities = Html.htmlentities

module Parse_tree = struct
  let of_channel ic : Cst.Impl.parse_tree =
    parse_inlines (Block_parser.Pre.of_channel ic)

  let of_string s : Cst.Impl.parse_tree =
    parse_inlines (Block_parser.Pre.of_string s)
end

let of_channel ic : Ast.Impl.doc =
  let cst : Cst.Impl.attributes Cst.Impl.block list =
    Parse_tree.of_channel ic
  in
  let ast = List.map Ast.Impl.of_cst_block cst in
  ast

let of_string s : Ast.Impl.doc =
  let cst : Cst.Impl.attributes Cst.Impl.block list = Parse_tree.of_string s in
  let ast = List.map Ast.Impl.of_cst_block cst in
  ast

let to_html ?auto_identifiers doc =
  Html.to_string (Html.of_doc ?auto_identifiers doc)

let to_sexp ast = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

module Print = Print

include Ast

type t = inline block list

let parse_inlines md =
  let parse_inline defs s = Parser.inline defs (Parser.P.of_string s) in
  let defs = Ast.defs md in
  let defs =
    List.map (fun def -> {def with Ast.label = Parser.normalize def.label}) defs
  in
  List.map (Ast.map (parse_inline defs)) md

let of_channel ic =
  let md = Block.Pre.of_channel ic in
  parse_inlines md

let of_string s =
  let md = Block.Pre.of_string s in
  parse_inlines md

let to_html doc =
  Html.to_html doc

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

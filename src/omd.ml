module Pre = Block.Pre

module Html = Html

include Ast

type doc = block list


let parse_inlines (md, defs) =
  let defs =
    let f (def : link_def) = {def with label = Parser.normalize def.label} in
    List.map f defs
  in
  List.map (Mapper.map (parse_inline defs)) md

let of_channel ic =
  parse_inlines (Pre.of_channel ic)

let of_string s =
  parse_inlines (Pre.of_string s)

let to_html doc =
  Html.to_string (Html.of_doc doc)

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

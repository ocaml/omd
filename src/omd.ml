include Ast

type t = Block.t list

let parse_inlines (md : Raw.t list) =
  let parse_inline defs s = Parser.inline defs (Parser.P.of_string s) in
  let defs = Raw.defs md in
  let defs =
    List.map (fun (def: string link_def) -> {def with label = Parser.normalize def.label}) defs
  in
  List.map (Mapper.map (parse_inline defs)) md

let of_channel ic =
  let md = Blocks.of_channel ic in
  parse_inlines md

let of_string s =
  let md = Blocks.of_string s in
  parse_inlines md

let to_html doc =
  Html.to_string (Html.of_doc doc)

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

module Pre = Block.Pre

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
  let md = Pre.of_channel ic in
  parse_inlines md

let of_string s =
  let md = Pre.of_string s in
  parse_inlines md

let to_html doc =
  let buf = Buffer.create 1024 in
  Html.add_to_buffer buf (Html.of_doc doc);
  Buffer.contents buf

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

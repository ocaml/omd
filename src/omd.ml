include Ast

type printer = Html.printer =
  {
    document: printer       -> Buffer.t -> inline block list -> unit;
    block: printer          -> Buffer.t -> inline block      -> unit;
    paragraph: printer      -> Buffer.t -> inline            -> unit;
    blockquote: printer     -> Buffer.t -> inline block list -> unit;
    list: printer           -> Buffer.t -> inline block_list -> unit;
    code_block: printer     -> Buffer.t -> code_block        -> unit;
    thematic_break: printer -> Buffer.t                      -> unit;
    html_block: printer     -> Buffer.t -> string            -> unit;
    heading: printer        -> Buffer.t -> int -> inline     -> unit;
    inline: printer         -> Buffer.t -> inline            -> unit;
    concat: printer         -> Buffer.t -> inline list       -> unit;
    text: printer           -> Buffer.t -> string            -> unit;
    emph: printer           -> Buffer.t -> emph              -> unit;
    code: printer           -> Buffer.t -> int -> string     -> unit;
    hard_break: printer     -> Buffer.t                      -> unit;
    soft_break: printer     -> Buffer.t                      -> unit;
    html: printer           -> Buffer.t -> string            -> unit;
    link: printer           -> Buffer.t -> link              -> unit;
    ref: printer            -> Buffer.t -> ref               -> unit;
  }

type t = inline block list

let parse_inlines md =
  let parse_inline defs s = Parser.inline defs (Parser.P.of_string s) in
  let defs = Ast.defs md in
  let defs =
    List.map (fun (def: string link_def) -> {def with Ast.label = Parser.normalize def.label}) defs
  in
  List.map (Ast.map (parse_inline defs)) md

let of_channel ic =
  let md = Block.Pre.of_channel ic in
  parse_inlines md

let of_string s =
  let md = Block.Pre.of_string s in
  parse_inlines md

let default_printer = Html.default_printer

let to_html ?printer doc =
  Html.to_html ?printer doc

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

include Ast

type printer = Html.printer =
  {
    document: printer       -> Buffer.t -> inline block list         -> unit;
    block: printer          -> Buffer.t -> inline block              -> unit;
    paragraph: printer      -> Buffer.t -> inline                    -> unit;
    blockquote: printer     -> Buffer.t -> inline block list         -> unit;
    list: printer           -> Buffer.t -> inline block Block_list.t -> unit;
    code_block: printer     -> Buffer.t -> Code_block.t              -> unit;
    thematic_break: printer -> Buffer.t                              -> unit;
    html_block: printer     -> Buffer.t -> string                    -> unit;
    heading: printer        -> Buffer.t -> inline Heading.t          -> unit;
    inline: printer         -> Buffer.t -> inline                    -> unit;
    concat: printer         -> Buffer.t -> inline list               -> unit;
    text: printer           -> Buffer.t -> string                    -> unit;
    emph: printer           -> Buffer.t -> inline Emph.t             -> unit;
    code: printer           -> Buffer.t -> int -> string             -> unit;
    hard_break: printer     -> Buffer.t                              -> unit;
    soft_break: printer     -> Buffer.t                              -> unit;
    html: printer           -> Buffer.t -> string                    -> unit;
    link: printer           -> Buffer.t -> inline Link.t             -> unit;
    ref: printer            -> Buffer.t -> inline Ref.t              -> unit;
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

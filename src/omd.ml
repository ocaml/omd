module Pre = Block.Pre

include Ast

type printer = Html.printer =
  {
    document: printer       -> Buffer.t -> Block.t list              -> unit;
    attributes: printer     -> Buffer.t -> Attributes.t              -> unit;
    block: printer          -> Buffer.t -> Block.t                   -> unit;
    paragraph: printer      -> Buffer.t -> Inline.t                  -> unit;
    blockquote: printer     -> Buffer.t -> Block.t list              -> unit;
    list: printer           -> Buffer.t -> Block.block_list          -> unit;
    code_block: printer     -> Buffer.t -> Block.code_block          -> unit;
    thematic_break: printer -> Buffer.t                              -> unit;
    html_block: printer     -> Buffer.t -> string                    -> unit;
    heading: printer        -> Buffer.t -> Block.heading             -> unit;
    def_list: printer       -> Buffer.t -> Block.def_list            -> unit;
    tag_block: printer      -> Buffer.t -> Block.tag_block           -> unit;
    inline: printer         -> Buffer.t -> Inline.t                  -> unit;
    concat: printer         -> Buffer.t -> Inline.t list             -> unit;
    text: printer           -> Buffer.t -> string                    -> unit;
    emph: printer           -> Buffer.t -> Inline.emph               -> unit;
    code: printer           -> Buffer.t -> Inline.code               -> unit;
    hard_break: printer     -> Buffer.t                              -> unit;
    soft_break: printer     -> Buffer.t                              -> unit;
    html: printer           -> Buffer.t -> string                    -> unit;
    link: printer           -> Buffer.t -> Inline.link               -> unit;
    ref: printer            -> Buffer.t -> Inline.ref                -> unit;
    tag: printer            -> Buffer.t -> Inline.tag                -> unit;
  }

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

let default_printer = Html.default_printer

let to_html ?printer doc =
  Html.to_html ?printer doc

let to_sexp ast =
  Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

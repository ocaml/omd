(** A markdown parser in OCaml. *)

module Attributes = Ast.Attributes

module Link_def = Ast.Link_def
module Block_list = Ast.Block_list
module Code_block = Ast.Code_block
module Heading = Ast.Heading
module Def_list = Ast.Def_list
module Tag_block = Ast.Tag_block

type 'a block = 'a Ast.block =
  | Paragraph of 'a
  | List of 'a block Block_list.t
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of 'a Heading.t
  | Code_block of Code_block.t
  | Html_block of string
  | Link_def of string Link_def.t
  | Def_list of 'a Def_list.t
  | Tag_block of 'a block Tag_block.t

module Emph = Ast.Emph
module Code = Ast.Code
module Link = Ast.Link
module Ref = Ast.Ref
module Tag = Ast.Tag

type inline = Ast.inline =
  | Concat of inline list
  | Text of string
  | Emph of inline Emph.t
  | Code of Code.t
  | Hard_break
  | Soft_break
  | Link of inline Link.t
  | Ref of inline Ref.t
  | Html of string
  | Tag of inline Tag.t

type t = inline block list
(** A markdown document *)

type printer = Printer.printer =
  {
    document: printer       -> Buffer.t -> inline block list         -> unit;
    attributes: printer     -> Buffer.t -> Attributes.t              -> unit;
    block: printer          -> Buffer.t -> inline block              -> unit;
    paragraph: printer      -> Buffer.t -> inline                    -> unit;
    blockquote: printer     -> Buffer.t -> inline block list         -> unit;
    list: printer           -> Buffer.t -> inline block Block_list.t -> unit;
    code_block: printer     -> Buffer.t -> Code_block.t              -> unit;
    thematic_break: printer -> Buffer.t                              -> unit;
    html_block: printer     -> Buffer.t -> string                    -> unit;
    heading: printer        -> Buffer.t -> inline Heading.t          -> unit;
    def_list: printer       -> Buffer.t -> inline Def_list.t         -> unit;
    tag_block: printer      -> Buffer.t -> inline block Tag_block.t  -> unit;
    inline: printer         -> Buffer.t -> inline                    -> unit;
    concat: printer         -> Buffer.t -> inline list               -> unit;
    text: printer           -> Buffer.t -> string                    -> unit;
    emph: printer           -> Buffer.t -> inline Emph.t             -> unit;
    code: printer           -> Buffer.t -> Code.t                    -> unit;
    hard_break: printer     -> Buffer.t                              -> unit;
    soft_break: printer     -> Buffer.t                              -> unit;
    html: printer           -> Buffer.t -> string                    -> unit;
    link: printer           -> Buffer.t -> inline Link.t             -> unit;
    ref: printer            -> Buffer.t -> inline Ref.t              -> unit;
    tag: printer            -> Buffer.t -> inline Tag.t              -> unit;
  }

val of_channel: in_channel -> t

val of_string: string -> t

val html_printer: printer

val latex_printer: printer

val to_html: ?printer:printer -> t -> string

val to_latex: ?printer:printer -> t -> string

val to_sexp: t -> string

(** A markdown parser in OCaml. *)

type 'a link_def = 'a Ast.link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

module Block_list = Ast.Block_list
module Code_block = Ast.Code_block

type 'a block = 'a Ast.block =
  | Paragraph of 'a
  | List of 'a block Block_list.t
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of Code_block.t
  | Html_block of string
  | Link_def of string link_def

module Emph = Ast.Emph
module Link = Ast.Link
module Ref = Ast.Ref

type inline = Ast.inline =
  | Concat of inline list
  | Text of string
  | Emph of inline Emph.t
  | Code of int * string
  | Hard_break
  | Soft_break
  | Link of inline Link.t
  | Ref of inline Ref.t
  | Html of string

type t = inline block list
(** A markdown document *)

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
    heading: printer        -> Buffer.t -> int -> inline             -> unit;
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

val of_channel: in_channel -> t

val of_string: string -> t

val default_printer: printer

val to_html: ?printer:printer -> t -> string

val to_sexp: t -> string

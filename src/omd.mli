(** A markdown parser in OCaml. *)

[@@@warning "-30"]

type list_kind = Ast.list_kind =
  | Ordered of int * char
  | Unordered of char

type list_style = Ast.list_style =
  | Loose
  | Tight

type 'a link_def = 'a Ast.link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type fenced_code_kind = Ast.fenced_code_kind =
  | Tilde
  | Backtick

type 'a block_list = 'a Ast.block_list =
  {
    list_kind: list_kind;
    list_style: list_style;
    blocks: 'a block list list;
  }
and code_block = Ast.code_block =
  {
    code_kind: fenced_code_kind option;
    code_label: string option;
    code_other: string option;
    code: string option;
  }
and 'a block = 'a Ast.block =
  | Paragraph of 'a
  | List of 'a block_list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of code_block
  | Html_block of string
  | Link_def of string link_def

type emph_kind = Ast.emph_kind =
  | Normal
  | Strong

type emph_style = Ast.emph_style =
  | Star
  | Underscore

type link_kind = Ast.link_kind =
  | Img
  | Url

type emph = Ast.emph =
  {
    kind: emph_kind;
    style: emph_style;
    md: inline;
  }
and link = Ast.link =
  {
    kind: link_kind;
    def: inline link_def;
  }
and ref = Ast.ref =
  {
    kind: link_kind;
    label: inline;
    def: string link_def;
  }
and inline = Ast.inline =
  | Concat of inline list
  | Text of string
  | Emph of emph
  | Code of int * string
  | Hard_break
  | Soft_break
  | Link of link
  | Ref of ref
  | Html of string

type t = inline block list
(** A markdown document *)

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

val of_channel: in_channel -> t

val of_string: string -> t

val default_printer: printer

val to_html: ?printer:printer -> t -> string

val to_sexp: t -> string

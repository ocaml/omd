(** A markdown parser in OCaml. *)

module Attributes : sig
  type t =
    {
      id: string option;
      classes: string list;
      attributes: (string * string) list;
    }
end

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
    attributes: Attributes.t;
  }

type link_kind =
  | Img
  | Url

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

module Inline : sig
  type emph =
    {
      style: emph_style;
      kind: emph_kind;
      content: t;
    }

  and code =
    {
      level: int;
      content: string;
      attributes: Attributes.t;
    }

  and link =
    {
      kind: link_kind;
      def: t link_def;
    }

  and ref =
    {
      kind: link_kind;
      label: t;
      def: string link_def;
    }

  and tag =
    {
      tag: string;
      content: t;
      attributes: Attributes.t
    }

  and t =
    | Concat of t list
    | Text of string
    | Emph of emph
    | Code of code
    | Hard_break
    | Soft_break
    | Link of link
    | Ref of ref
    | Html of string
    | Tag of tag
end

type block_list_kind =
  | Ordered of int * char
  | Unordered of char

type block_list_style =
  | Loose
  | Tight

type code_block_kind =
  | Tilde
  | Backtick

module Block : sig
  type block_list =
    {
      kind: block_list_kind;
      style: block_list_style;
      blocks: t list list;
    }

  and code_block =
    {
      kind: code_block_kind option;
      label: string option;
      other: string option;
      code: string option;
      attributes: Attributes.t;
    }

  and heading =
    {
      level: int;
      text: Inline.t;
      attributes: Attributes.t;
    }

  and def_elt =
    {
      term: Inline.t;
      defs: Inline.t list;
    }

  and def_list =
    {
      content: def_elt list
    }

  and tag_block =
    {
      tag: string;
      content: t list;
      attributes: Attributes.t
    }

  and t =
    | Paragraph of Inline.t
    | List of block_list
    | Blockquote of t list
    | Thematic_break
    | Heading of heading
    | Code_block of code_block
    | Html_block of string
    | Link_def of string link_def
    | Def_list of def_list
    | Tag_block of tag_block
end

type t = Block.t list
(** A markdown document *)

type printer =
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

val of_channel: in_channel -> t

val of_string: string -> t

val default_printer: printer

val to_html: ?printer:printer -> t -> string

val to_sexp: t -> string

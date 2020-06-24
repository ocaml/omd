(** A markdown parser in OCaml. *)

type attribute =
  string * string

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
    attributes: attribute list;
  }

module Inline : sig
  type code =
    {
      content: string;
      attributes: attribute list;
    }

  and t =
    | Concat of t list
    | Text of string
    | Emph of t
    | Strong of t
    | Code of code
    | Hard_break
    | Soft_break
    | Link of t link_def
    | Image of t link_def
    | Html of string
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
      attributes: attribute list;
    }

  and heading =
    {
      level: int;
      text: Inline.t;
      attributes: attribute list;
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
end

type t = Block.t list
(** A markdown document *)

val of_channel: in_channel -> t

val of_string: string -> t

val to_html: t -> string

val to_sexp: t -> string

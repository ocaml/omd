(** A markdown parser in OCaml. *)

type attributes =
  (string * string) list

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

module Inline : sig
  type code =
    {
      content: string;
    }

  and t =
    {
      il_desc: t_desc;
      il_attributes: attributes;
    }

  and t_desc =
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

type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

module Block : sig
  type def_elt =
    {
      term: Inline.t;
      defs: Inline.t list;
    }

  and def_list =
    {
      content: def_elt list
    }

  and t =
    {
      bl_desc: t_desc;
      bl_attributes: attributes;
    }

  and t_desc =
    | Paragraph of Inline.t
    | List of list_type * list_spacing * t list list
    | Blockquote of t list
    | Thematic_break
    | Heading of int * Inline.t
    | Code_block of string * string
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

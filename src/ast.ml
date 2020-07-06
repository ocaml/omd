type attributes =
  (string * string) list

type list_type =
  | Ordered of int * char
  | Bullet of char

type list_spacing =
  | Loose
  | Tight

let same_block_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Bullet c1, Bullet c2 -> c1 = c2
  | _ -> false

type link_def =
  {
    label: string;
    destination: string;
    title: string option;
    attributes: attributes;
  }

module type T = sig
  type t
end

module MakeBlock (I : T) = struct
  type def_elt =
    {
      term: I.t;
      defs: I.t list;
    }

  and block =
    {
      bl_desc: block_desc;
      bl_attributes: attributes;
    }

  and block_desc =
    | Paragraph of I.t
    | List of list_type * list_spacing * block list list
    | Blockquote of block list
    | Thematic_break
    | Heading of int * I.t
    | Code_block of string * string
    | Html_block of string
    | Definition_list of def_elt list
end

type link =
  {
    label: inline;
    destination: string;
    title: string option;
  }

and inline =
  {
    il_desc: inline_desc;
    il_attributes: attributes;
  }

and inline_desc =
  | Concat of inline list
  | Text of string
  | Emph of inline
  | Strong of inline
  | Code of string
  | Hard_break
  | Soft_break
  | Link of link
  | Image of link
  | Html of string

module Raw = MakeBlock (String)

module Inline = struct type t = inline end

include MakeBlock (Inline)

module MakeMapper (Src : T) (Dst : T) = struct
  module SrcBlock = MakeBlock(Src)
  module DstBlock = MakeBlock(Dst)

  let rec map (f : Src.t -> Dst.t) : SrcBlock.block -> DstBlock.block =
    fun {bl_desc; bl_attributes} ->
    let bl_desc =
      match bl_desc with
      | SrcBlock.Paragraph x -> DstBlock.Paragraph (f x)
      | List (ty, sp, bl) ->
          List (ty, sp, List.map (List.map (map f)) bl)
      | Blockquote xs ->
          Blockquote (List.map (map f) xs)
      | Thematic_break ->
          Thematic_break
      | Heading (level, text) ->
          Heading (level, f text)
      | Definition_list l ->
          let f {SrcBlock.term; defs} = {DstBlock.term = f term; defs = List.map f defs} in
          Definition_list (List.map f l)
      | Code_block (label, code) ->
          Code_block (label, code)
      | Html_block x ->
          Html_block x
    in
    {bl_desc; bl_attributes}
end

module Mapper = MakeMapper (String) (Inline)

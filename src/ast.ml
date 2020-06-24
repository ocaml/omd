type attribute =
  string * string

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
    attributes: attribute list;
  }

type block_list_kind =
  | Ordered of int * char
  | Unordered of char

let same_block_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Unordered c1, Unordered c2 -> c1 = c2
  | _ -> false

type block_list_style =
  | Loose
  | Tight

type code_block_kind =
  | Tilde
  | Backtick

module type T = sig
  type t
end

module MakeBlock (Inline : T) = struct
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

  let defs ast =
    let rec loop acc = function
      | List l -> List.fold_left (List.fold_left loop) acc l.blocks
      | Blockquote l -> List.fold_left loop acc l
      | Paragraph _ | Thematic_break | Heading _
      | Def_list _ | Code_block _ | Html_block _ -> acc
      | Link_def def -> def :: acc
    in
    List.rev (List.fold_left loop [] ast)
end

type link_kind =
  | Img
  | Url

module Inline = struct
  type code =
    {
      content: string;
      attributes: attribute list;
    }

  and link =
    {
      kind: link_kind;
      def: t link_def;
    }

  and t =
    | Concat of t list
    | Text of string
    | Emph of t
    | Strong of t
    | Code of code
    | Hard_break
    | Soft_break
    | Link of link
    | Html of string
end

module Raw = MakeBlock (String)

module Block = MakeBlock (Inline)

module MakeMapper (Src : T) (Dst : T) = struct
  module SrcBlock = MakeBlock(Src)
  module DstBlock = MakeBlock(Dst)

  let rec map (f : Src.t -> Dst.t) : SrcBlock.t -> DstBlock.t = function
    | SrcBlock.Paragraph x -> DstBlock.Paragraph (f x)
    | List {kind; style; blocks} ->
        List  {kind; style; blocks = List.map (List.map (map f)) blocks}
    | Blockquote xs ->
        Blockquote (List.map (map f) xs)
    | Thematic_break ->
        Thematic_break
    | Heading {level; text; attributes} ->
        Heading {level; text = f text; attributes}
    | Def_list {content} ->
        let f {SrcBlock.term; defs} = {DstBlock.term = f term; defs = List.map f defs} in
        Def_list {content = List.map f content}
    | Code_block {kind; label; other; code; attributes} ->
        Code_block {kind; label; other; code; attributes}
    | Html_block x ->
        Html_block x
    | Link_def x ->
        Link_def x
end

module Mapper = MakeMapper (String) (Inline)

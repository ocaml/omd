type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type attributes =
  {
    id: string option;
    classes: string list;
    attributes: (string * string) list
  }

let empty_attributes = {id=None; classes=[]; attributes=[]}

module Block_list = struct
  type kind =
    | Ordered of int * char
    | Unordered of char

  type style =
    | Loose
    | Tight

  type 'block t =
  {
    kind: kind;
    style: style;
    blocks: 'block list list;
  }
end

module Code_block = struct
  type kind =
    | Tilde
    | Backtick

  type t =
    {
      kind: kind option;
      label: string option;
      other: string option;
      attributes: attributes;
      code: string option;
    }
end

module Heading = struct
  type 'block t =
    {
      level: int;
      attributes: attributes;
      text: 'block;
    }
end

type 'a block =
  | Paragraph of 'a
  | List of 'a block Block_list.t
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of 'a Heading.t
  | Code_block of Code_block.t
  | Html_block of string
  | Link_def of string link_def

module Emph = struct
  type kind =
    | Normal
    | Strong

  type style =
    | Star
    | Underscore

  type 'inline t =
  {
    style: style;
    kind: kind;
    content: 'inline
  }
end

type link_kind =
  | Img
  | Url

module Link = struct
  type kind = link_kind

  type 'inline t =
  {
    kind: kind;
    def: 'inline link_def;
  }
end

module Ref = struct
  type kind = link_kind

  type 'inline t =
  {
    kind: kind;
    label: 'inline;
    def: string link_def;
  }
end

type inline =
  | Concat of inline list
  | Text of string
  | Emph of inline Emph.t
  | Code of int * string
  | Hard_break
  | Soft_break
  | Link of inline Link.t
  | Ref of inline Ref.t
  | Html of string

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List l -> List  {l with blocks = List.map (List.map (map f)) l.blocks}
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading h -> Heading {h with text = f h.text}
  | Code_block _ | Html_block _ | Link_def _ as x -> x

let defs ast =
  let rec loop acc = function
    | List l -> List.fold_left (List.fold_left loop) acc l.blocks
    | Blockquote l -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

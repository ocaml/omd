module Attributes =
struct
  type t =
    {
      id: string option;
      classes: string list;
      attributes: (string * string) list;
    }

  let empty = {id=None; classes=[]; attributes=[]}
end

module Link_def =
struct
  type 'a t =
    {
      label: 'a;
      destination: string;
      title: string option;
      attributes: Attributes.t;
    }
end

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
      code: string option;
      attributes: Attributes.t;
    }
end

module Heading = struct
  type 'block t =
    {
      level: int;
      text: 'block;
      attributes: Attributes.t;
    }
end

module Tag_block = struct
  type 'block t =
  {
    tag: string;
    content: 'block list;
    attributes: Attributes.t
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
  | Link_def of string Link_def.t
  | Tag_block of 'a block Tag_block.t

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
    content: 'inline;
  }
end

module Code = struct
  type t =
  {
    level: int;
    content: string;
    attributes: Attributes.t;
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
    def: 'inline Link_def.t;
  }
end

module Ref = struct
  type kind = link_kind

  type 'inline t =
  {
    kind: kind;
    label: 'inline;
    def: string Link_def.t;
  }
end

module Tag = struct
  type 'inline t =
  {
    tag: string;
    content: 'inline;
    attributes: Attributes.t
  }
end

type inline =
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

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List l -> List  {l with blocks = List.map (List.map (map f)) l.blocks}
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading h -> Heading {h with text = f h.text}
  | Tag_block t -> Tag_block {t with content = List.map (map f) t.content}
  | Code_block _ | Html_block _ | Link_def _ as x -> x

let defs ast =
  let rec loop acc = function
    | List l -> List.fold_left (List.fold_left loop) acc l.blocks
    | Blockquote l | Tag_block {content = l; _} -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

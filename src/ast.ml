[@@@warning "-30"]

type list_kind =
  | Ordered of int * char
  | Unordered of char

type list_style =
  | Loose
  | Tight

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type fenced_code_kind =
  | Tilde
  | Backtick

type 'a block_list =
  {
    list_kind: list_kind;
    list_style: list_style;
    blocks: 'a block list list;
  }
and code_block =
  {
    code_kind: fenced_code_kind option;
    code_label: string option;
    code_other: string option;
    code: string option;
  }
and 'a block =
  | Paragraph of 'a
  | List of 'a block_list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of code_block
  | Html_block of string
  | Link_def of string link_def

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

type link_kind =
  | Img
  | Url

type emph =
  {
    kind: emph_kind;
    style: emph_style;
    md: inline;
  }
and link =
  {
    kind: link_kind;
    def: inline link_def;
  }
and ref =
  {
    kind: link_kind;
    label: inline;
    def: string link_def;
  }
and inline =
  | Concat of inline list
  | Text of string
  | Emph of emph
  | Code of int * string
  | Hard_break
  | Soft_break
  | Link of link
  | Ref of ref
  | Html of string

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List l -> List  {l with blocks = List.map (List.map (map f)) l.blocks}
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading (i, x) -> Heading (i, f x)
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

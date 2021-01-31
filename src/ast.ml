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

type def_elt =
  {
    term: inline;
    defs: inline list;
  }

and block =
  {
    bl_desc: block_desc;
    bl_attributes: attributes;
  }

and block_desc =
  | Paragraph of inline
  | List of list_type * list_spacing * block list list
  | Blockquote of block list
  | Thematic_break
  | Heading of int * inline
  | Code_block of string * string
  | Html_block of string
  | Definition_list of def_elt list

type doc = block list

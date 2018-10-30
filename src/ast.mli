type list_kind =
  | Ordered of int * char
  | Unordered of char

type list_style =
  | Loose
  | Tight

type 'a block =
  | Paragraph of 'a
  | List of list_kind * list_style * 'a block list list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of string * string option
  | Html_block of string

val map : ('a -> 'b) -> 'a block -> 'b block

val same_list_kind : list_kind -> list_kind -> bool

type inline =
  | Cat of inline list
  | Text of string
  | Emph of inline
  | Bold of inline
  | Code of string
  | Hard_break
  | Soft_break
  | Url of inline * string * string option
  (* | Ref of string * string *)
  (* | Img_ref of string * string *)
  | Html of string
  | Img of string * string * string

val cat : inline list -> inline

val normalize_label : inline -> string

type link_def =
  {
    label: string;
    destination: string;
    title: string option;
  }

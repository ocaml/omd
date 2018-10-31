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

type 'a block =
  | Paragraph of 'a
  | List of list_kind * list_style * 'a block list list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of (fenced_code_kind * string) option * string option
  | Html_block of string
  | Link_def of 'a link_def

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

type inline =
  | Concat of inline list
  | Text of string
  | Emph of emph_kind * emph_style * inline
  | Code of string
  | Hard_break
  | Soft_break
  | Url of inline * string * string option
  (* | Ref of string * string *)
  (* | Img_ref of string * string *)
  | Html of string
  | Img of string * string * string

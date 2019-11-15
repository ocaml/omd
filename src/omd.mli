(** A markdown parser in OCaml. *)

module Attributes = Ast.Attributes

module Link_def = Ast.Link_def
module Block_list = Ast.Block_list
module Code_block = Ast.Code_block
module Heading = Ast.Heading
module Def_list = Ast.Def_list
module Tag_block = Ast.Tag_block

type 'a block = 'a Ast.block =
  | Paragraph of 'a
  | List of 'a block Block_list.t
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of 'a Heading.t
  | Code_block of Code_block.t
  | Html_block of string
  | Link_def of string Link_def.t
  | Def_list of 'a Def_list.t
  | Tag_block of 'a block Tag_block.t

module Emph = Ast.Emph
module Code = Ast.Code
module Link = Ast.Link
module Ref = Ast.Ref
module Tag = Ast.Tag

type inline = Ast.inline =
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

type t = inline block list
(** A markdown document *)

val of_channel: in_channel -> t

val of_string: string -> t

module Mapper = Ast.Mapper
module Iterator = Ast.Iterator

type printer = (string -> unit) Iterator.t

val default_printer: printer

val to_html: ?printer:printer -> t -> string

val to_sexp: t -> string

type 'a t =
  | Paragraph of 'a
  | List of 'a t list list
  | Blockquote of 'a t list
  | Thematic_break
  | Atx_heading of int * string
  | Code_block of string * 'a
  | Html_block of 'a

val map: f:('a -> 'b) -> 'a t -> 'b t

module Parser : sig
  type blocks
  type t

  val empty : t
  val process : t -> string -> t
  val finish : t -> blocks
end with type blocks := string t list

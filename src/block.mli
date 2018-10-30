type blocks = string Ast.block list
type t

val empty : t
val process : t -> string -> t
val finish : t -> Inline.link_def list * blocks

val to_html : (Buffer.t -> 'a -> unit) -> 'a Ast.block list -> string

val of_channel : in_channel -> Inline.link_def list * string Ast.block list

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Ast.block -> unit

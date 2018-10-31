type blocks = string Ast.block list
type t

val empty : t
val process : t -> string -> t
val finish : t -> blocks

val to_html : (Buffer.t -> 'a -> unit) -> 'a Ast.block list -> string

val of_channel : in_channel -> string Ast.block list

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Ast.block -> unit

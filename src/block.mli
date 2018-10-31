type blocks = string Ast.block list
type t

val empty : t
val process : t -> string -> t
val finish : t -> blocks

val of_channel : in_channel -> string Ast.block list

type t

val empty: t
val process: t -> string -> t
val finish: t -> Ast.Raw.t list

val of_channel: in_channel -> Ast.Raw.t list
val of_string: string -> Ast.Raw.t list

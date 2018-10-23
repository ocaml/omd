type t

val of_string : string -> t
val to_string : t -> string
val offset : int -> t -> t
val lexbuf : t -> Lexing.lexbuf
val contains : string -> t -> bool

val head : t -> (char * t) option
val tail : t -> t

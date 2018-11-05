type t

val of_string: string -> t
val to_string: t -> string
val offset: int -> t -> t
val lexbuf: t -> Lexing.lexbuf
val contains: string -> t -> bool

val print: Format.formatter -> t -> unit

val head: ?rev:unit -> t -> char option
val tail: ?rev:unit -> t -> t

val heads: int -> t -> char list * t

val for_all: (char -> bool) -> t -> bool
val exists: (char -> bool) -> t -> bool
val is_empty: t -> bool
val length: t -> int

val sub: len:int -> t -> t

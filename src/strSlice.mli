(* Implementation of string slices over a base string via an offset *)

type t

val of_string : ?off:int -> string -> t
val to_string : t -> string
val offset : int -> t -> t
val lexbuf : t -> Lexing.lexbuf
val contains : string -> t -> bool
val print : Format.formatter -> t -> unit
val head : t -> char option
val tail : t -> t

val last : t -> char option
(** [last s] is the [Some c] if [c] is the last character of [s], or else [None] if [s] is empty *)

val drop_last : t -> t
(** [drop_last s] is the [s] without its last character *)

val take : int -> t -> char list
(** [take n s] is a list of the first [n] characters of [s] *)

val drop : int -> t -> t
(** [drop n s] is [s] with the first [n] characters dropped *)

val for_all : (char -> bool) -> t -> bool
val exists : (char -> bool) -> t -> bool
val is_empty : t -> bool
val get_offset : t -> int
val length : t -> int
val sub : len:int -> t -> t

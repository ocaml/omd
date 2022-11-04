(* Implementation of string slices over a base string via an offset *)

type t

val of_string : ?off:int -> string -> t
val to_string : t -> string
val offset : int -> t -> t
val lexbuf : t -> Lexing.lexbuf
val contains : string -> t -> bool
val length : t -> int

val index : (char -> bool) -> t -> int option
(** [index c s] is [Some i] where [i] is the index of the character in [s] for
    which [f] is first true, or [None] if [f] holds for no characters in [s]. *)

val index_unescaped : char -> t -> int option
(** [index_unescaped c s] is [Some i] where [i] index of the first
    occurrence of the character [c] in [s] that is not preceeded by a
    backslash ['\\'] and not within a verbatim inline, or [None] if
    there is no such [c] in [s]. *)

val print : Format.formatter -> t -> unit
val head : t -> char option
val tail : t -> t

val uncons : t -> (char * t) option
(** [uncons s] is [Some (h, t)] where [h] is [head s] and [t] is [tail s],
    or [None] if [is_empty s] *)

val last : t -> char option
(** [last s] is the [Some c] if [c] is the last character of [s], or else [None] if [s] is empty *)

val drop_last : t -> t
(** [drop_last s] is the [s] without its last character *)

val take : int -> t -> char list
(** [take n s] is a list of the first [n] characters of [s] *)

val take_n : int -> t -> t
(** [take_n n s] returns the slice consisting of the first [n]
    characters of [s]. *)

val drop : int -> t -> t
(** [drop n s] is [s] with the first [n] characters dropped *)

val drop_while : (char -> bool) -> t -> t
(** [drop_while f s] is [s] with the longest prefix for which [f] is true for
    every character dropped *)

val drop_last_while : (char -> bool) -> t -> t
(** [drop_last_while f s] is [s] with the longest suffix for which [f] is true for
    every character dropped *)

val split_at : (char -> bool) -> t -> t * t
(** [split_at f s] is [(taken, rest)] where [taken] is the prefix of [s] for
    which [f] is [false] and [rest] is remainder, including the first character
    for which [f] is [true].

    E.g.,

    {[
      let () =
        let f x = x = 'c' in
        let before, rest = split_at f (of_string "abcdef") in
        assert ("ab" = to_string before);
        assert ("cdef" = to_string rest);
        let before, rest = split_at f (of_string "cab") in
        assert ("" = to_string before);
        assert ("cab" = to_string rest);
        let before, rest = split_at f (of_string "aaa") in
        assert ("aaa" = to_string before);
        assert ("" = to_string rest)
    ]}
*)

val fold_left : (char -> 'a -> 'a) -> 'a -> t -> 'a
val for_all : (char -> bool) -> t -> bool
val exists : (char -> bool) -> t -> bool
val is_empty : t -> bool
val get_offset : t -> int
val sub : len:int -> t -> t

val trim : t -> t
(** [trim s] returns the slice that skips any whitespace at the start
    or the end of [s]. *)

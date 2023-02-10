type t

val empty : t

val touch : string -> t -> int * t
(** Bump the frequency count for the given string. 
    It returns the previous count (before bumping) *)

type 'a t

val pop : 'a t -> ('a * 'a t) option

val push : 'a -> 'a t -> 'a t

val to_list : 'a t -> 'a list

val empty : 'a t

val is_empty : 'a t -> bool

val map : ('a -> 'b) -> 'a t -> 'b t

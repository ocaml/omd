open Ast

val same_list_kind: list_kind -> list_kind -> bool

type 'a t = 'a block

val map: ('a -> 'b) -> 'a t -> 'b t

val defs: 'a t list -> string link_def list

module Pre : sig
  type block
  type t

  val empty : t
  val process : t -> string -> t
  val finish : t -> block list

  val of_channel : in_channel -> block list
end with type block := string t

open Ast

val same_list_kind: Block_list.kind -> Block_list.kind -> bool

type 'a t = 'a block

module Pre : sig
  type block
  type t

  val empty: t
  val process: t -> string -> t
  val finish: t -> block list

  val of_channel: in_channel -> block list
  val of_string: string -> block list
end with type block := string t

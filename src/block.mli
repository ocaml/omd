open Ast

val same_list_kind: block_list_kind -> block_list_kind -> bool

module Pre : sig
  type t

  val empty: t
  val process: t -> string -> t
  val finish: t -> block list

  val of_channel: in_channel -> block list
  val of_string: string -> block list
end

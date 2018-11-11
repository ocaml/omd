open Ast

type t = inline

val concat: t list -> t

val normalize: t -> t

val condense_ws: string -> string

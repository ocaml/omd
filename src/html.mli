type attribute =
  string * string

type element_type =
  | Inline
  | Block

type t =
  | Element of element_type * string * attribute list * t option
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

val of_doc : Ast.Block.t list -> t

val add_to_buffer : Buffer.t -> t -> unit

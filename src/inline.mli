type t =
  | Cat of t list
  | Text of string
  | Emph of t
  | Bold of t
  | Code of string
  | Hard_break
  | Soft_break
  | Url of t * string * string option
  (* | Ref of string * string *)
  (* | Img_ref of string * string *)
  | Html of string
  (* | Raw of string *)
  | Img of string * string * string

val print : Format.formatter -> t -> unit

val html_of_md : Buffer.t -> t -> unit
val markdown_of_md : t -> string

val parse : string -> t

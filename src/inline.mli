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

type link_def =
  {
    label: string;
    destination: string;
    title: string option;
  }

val parse : link_def list -> string -> t

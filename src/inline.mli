type delim =
  | Ws
  | Punct
  | Other

type emph =
  | Star
  | Underscore

type t =
  | Bang_left_bracket
  | Left_bracket
  | Emph of delim * delim * emph * int
  | R of Ast.inline

val is_opener : t -> bool
val is_closer : t -> bool
val classify_delim : char -> delim

val parse_emph : t list -> Ast.inline list

val html_of_md : Buffer.t -> Ast.inline -> unit
val markdown_of_md : Ast.inline -> string
(* val parse : Ast.link_def list -> string -> Ast.inline *)

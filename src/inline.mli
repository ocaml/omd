type delim =
  | Ws
  | Punct
  | Other

type t =
  | Bang_left_bracket
  | Left_bracket
  | Emph of delim * delim * Ast.emph_style * int
  | R of Ast.inline

val is_opener : t -> bool
val is_closer : t -> bool
val classify_delim : char -> delim

val parse_emph : t list -> Ast.inline list

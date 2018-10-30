val html_of_md : Buffer.t -> Ast.inline -> unit
val markdown_of_md : Ast.inline -> string
val parse : Ast.link_def list -> string -> Ast.inline

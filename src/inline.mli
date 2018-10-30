val print : Format.formatter -> Ast.inline -> unit

val html_of_md : Buffer.t -> Ast.inline -> unit
val markdown_of_md : Ast.inline -> string

type link_def =
  {
    label: string;
    destination: string;
    title: string option;
  }

val parse : link_def list -> string -> Ast.inline

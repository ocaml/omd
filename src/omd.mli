(** {1 A markdown parser in OCaml} *)

(** {2 The document model}

    The following types define the AST representing Omd's document model. *)

include Ast.Intf

(** {2 Generating and constructing tables of contents} *)

val headers :
  ?remove_links:bool -> 'attr block list -> ('attr * int * 'attr inline) list

val toc : ?start:int list -> ?depth:int -> doc -> doc

(** {2 Converting to and from documents} *)

val of_channel : in_channel -> doc
val of_string : string -> doc
val to_html : doc -> string
val to_sexp : doc -> string

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val make_paragraphs : Omd_representation.t -> Omd_representation.t
(** Since [Omd_parser.parse] doesn't build paragraphs, if you want 
    Markdown-style paragraphs, you need to apply this function
    to the result of [Omd_parser.parse]. *)

val html_of_md :
  ?pindent:bool ->
  ?nl2br:bool -> 
Omd_representation.t -> string
(** [html_of_md md] returns a string containing the HTML version of
    [md]. Note that [md] uses the internal representation of
    Markdown. *)

val headers_of_md :
  Omd_representation.t ->
  (Omd_representation.element * string * string) list
(** [headers_of_md md] returns a list of 3-tuples; in each of them the
    first element is the header (e.g., [H1(foo)]), the second is the
    HTML id (as produced by [html_of_md]), and the thrid element is
    the HTML version of [foo].  The third elements of those 3-tuples
    exist because if you use [html_and_headers_of_md], then you have
    the guarantee that the HTML version of [foo] is the same for
    both the headers and the HTML version of [md]. *)

val html_and_headers_of_md :
  ?pindent:bool ->
  ?nl2br:bool ->
  Omd_representation.t ->
  string *
    (Omd_representation.element * Omd_utils.StringSet.elt * string) list
(** [html_and_headers_of_md md] is the same as [(html_of_md md,
    headers_of_md md)] except that it's two times faster.
    If you need headers and html, don't use [html_of_md]
    and [headers_of_md] but this function instead.
    Optional parameters
*)

val text_of_md : Omd_representation.t -> string
(** [text_of_md md] is basically the same as [html_of_md md] but without
    the HTML tags in the output. *)

val markdown_of_md : Omd_representation.t -> string
(** [markdown_of_md md] is basically the same as [html_of_md md] but
    with the output in Markdown syntax rather than HTML. *)

val sexpr_of_md : Omd_representation.t -> string
(** [sexpr_of_md md] is basically the same as [html_of_md md] but with
    the output in s-expressions rather than HTML. This is mainly used
    for debugging. *)

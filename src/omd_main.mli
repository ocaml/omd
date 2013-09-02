(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val remove_comments : Omd.tok list -> Omd.tok list
(** [remove_comments l] returns [l] without OMD comments. *)

val remove_endline_comments : Omd.tok list -> Omd.tok list
(** [remove_endline_comments l] returns [l] without OMD endline-comments. *)

val preprocess_functions : (Omd.tok list -> Omd.tok list) list ref
(** [preprocess_functions] contains the list of preprocessing functions *)

val preprocess : Omd.tok list -> Omd.tok list
(** [preprocess l] returns [l] to which all preprocessing functions
    (in reference [preprocess_functions]) have been applied. *)

val otoc : bool ref
(** flag: output the table of contents only. *)

val toc : bool ref
(** flag: replace "*Table of contents*" by the table of contents. *)

val omarkdown : bool ref
(** flag: output Markdown instead of HTML. *)

val notags : bool ref
(** flag: output HTML but without HTML tags, so it's not really HTML anymore. *)

val toc_depth : int ref
(** flag: depth of table of contents *) 

val toc_start : int ref
(** flag: first header level for table of contents *) 

val nl2br : bool ref
(** flag: convert newlines to "<br/>" when output is HTML *)

val protect_html_comments : bool ref
(** flag: for multiple dashes in HTML comments, replace dashes by &#45;  *)

val make_toc :
  ?start_level:int -> ?depth:int -> Omd_representation.t -> Omd.t
(** [make_toc ?(start_level=1) ?(depth=2) md] returns a table of
    contents when [md] is a list of section headers. If [md] contains
    tags other than section header tags, an exception is raised.
 *)

val patch_html_comments : Omd.element list -> Omd.element list
(** [patch_html_comments l] returns the list [l] where
    all [Html_comments s] have been converted to [Html_comments s'],
    where [s'] means [s] with dashes replaced by &#45; except for
    single dashes (which are left untouched).

    N.B. It seems that it's not valid to have double dashes inside HTML comments
    (cf. http://validator.w3.org/check). So one way to make life somewhat easier
    is to patch the comments and transform inner dashed to &#45;.  *)

val tag_toc : Omd.tok list -> Omd.tok list
(** [tag_toc l] returns [l] where *Table of contents* has been replaced
    by a tag that can generate a table of contents. *)

val main : unit -> unit
(** main function *)

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val gh_uemph_or_bold_style : bool ref
val blind_html : bool ref
val strict_html : bool ref

val htmlcodes_set : Omd_utils.StringSet.t
(** set of known HTML codes *)

val inline_htmltags_set : Omd_utils.StringSet.t
(** set of known inline HTML tags *)

val htmltags_set : Omd_utils.StringSet.t
(** All known HTML tags *)

val unindent_rev :
  int ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
(** [unindent_rev n l] returns the same couple as [unindent n l]
    except that the first element (which is a list) is reversed. *)

val unindent :
  int ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
(** [unindent n l] returns [(unindented, rest)] where [unindented] is
    the consecutive lines of [l] that are indented with at least [n]
    spaces, and de-indented by [n] spaces. If [l] starts with a line
    that is indented by less than [n] spaces, then it returns [([], l)].
*)


val is_blank : Omd_representation.tok list -> bool
(** [is_blank l] returns [true] if [l] only contains blanks, which are
    spaces and newlines. *)

val semph_or_bold :
  int ->
  Omd_representation.tok list ->
  (Omd_representation.tok list * Omd_representation.tok list) option
(** [semph_or_bold n l] returns [None] if [l] doesn't start with
    a bold/emph phrase (marked using stars), else it returns [Some(x,y)]
    where [x] is the emph and/or bold phrase at the beginning of [l]
    and [y] is the rest of [l]. *)

val sm_uemph_or_bold :
  int ->
  Omd_representation.tok list ->
  (Omd_representation.tok list * Omd_representation.tok list) option
(** [sm_uemph_or_bold n l] returns [None] if [l] doesn't start with
    a bold/emph phrase (marked using underscores), else it returns [Some(x,y)]
    where [x] is the emph and/or bold phrase at the beginning of [l]
    and [y] is the rest of [l]. *)

val gh_uemph_or_bold :
  int ->
  Omd_representation.tok list ->
  (Omd_representation.tok list * Omd_representation.tok list) option
(** [gh_uemph_or_bold n l] returns [None] if [l] doesn't start with
    a bold/emph phrase (marked using underscores), else it returns [Some(x,y)]
    where [x] is the emph and/or bold phrase at the beginning of [l]
    and [y] is the rest of [l]. *)

val uemph_or_bold :
  int ->
  Omd_representation.tok list ->
  (Omd_representation.tok list * Omd_representation.tok list) option
(** [uemph_or_bold n l] returns [None] if [l] doesn't start with a
    bold/emph phrase (marked using underscores), else it returns
    [Some(x,y)] where [x] is the emph and/or bold phrase at the
    beginning of [l] and [y] is the rest of [l]. N.B. if
    [!gh_uemph_or_bold_style] then in Github style (i.e., underscores
    inside words are considered as underscores). *)

val eat_blank : Omd_representation.tok list -> Omd_representation.tok list
(** [eat_blank l] returns [l] where all blanks at the beginning of the
    list have been removed (it stops removing as soon as it meets an element
    that is not a blank). Blanks are spaces and newlines only. *)

val is_space_or_equal : Omd_representation.tok -> bool
val is_space_or_minus : Omd_representation.tok -> bool
val setext_title :
  Omd_representation.tok list ->
  (Omd_representation.tok list * Omd_representation.tok list) option
val tag_maybe_h1 :
  ('a list -> 'b list -> Omd_representation.tok list -> Omd_representation.t) ->
  Omd_representation.tok
val tag_maybe_h2 :
  ('a list -> 'b list -> Omd_representation.tok list -> Omd_representation.t) ->
  Omd_representation.tok
val tag_md : Omd_representation.element list -> Omd_representation.tok


val tag_setext :
  (Omd_representation.t -> Omd_representation.tok list -> Omd_representation.tok list -> Omd_representation.t) ->
  Omd_representation.tok list -> Omd_representation.tok list
(** Let's tag the lines that *might* be titles using setext-style.
    "might" because if they are, for instance, in a code section,
    then they are not titles at all. *)


val hr_m : Omd_representation.tok list -> Omd_representation.tok list option
(** [hr_m l] returns [Some nl] where [nl] is the remaining of [l] if [l]
    contains a horizontal rule drawn with dashes. If it doesn't, then
    returns [None].*)

val hr_s : Omd_representation.tok list -> Omd_representation.tok list option
(** [hr_s l] returns [Some nl] where [nl] is the remaining of [l] if [l]
    contains a horizontal rule drawn with stars. If it doesn't, then
    returns [None].*)


exception NL_exception
exception Premature_ending
val read_until_gt :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_lt :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_cparenth :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_oparenth :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_dq :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_q :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_obracket :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_cbracket :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_space :
  ?no_nl:bool ->
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_until_newline :
  Omd_representation.tok list ->
  Omd_representation.tok list * Omd_representation.tok list
val read_title :
  ('a list -> 'b list -> Omd_representation.tok list -> Omd_representation.t) ->
  int ->
  Omd_representation.element list ->
  'c ->
  Omd_representation.tok list ->
  Omd_representation.element list * Omd_representation.tok list *
  Omd_representation.tok list


val maybe_extension :
  ('a -> 'b -> 'c -> ('a * 'b * 'c) option) list ->
  'a -> 'b -> 'c -> ('a * 'b * 'c) option
(** [maybe_extension e r p l] returns None if there is no extension or
    if extensions haven't had  any effect, returns Some(nr, np, nl) if
    at least one extension has applied successfully. *)

val emailstyle_quoting :
  ('a list -> 'b list -> Omd_representation.tok list -> Omd_representation.t) ->
  Omd_representation.element list ->
  'c ->
  Omd_representation.tok list ->
  Omd_representation.element list * Omd_representation.tok list *
  Omd_representation.tok list
val maybe_reference :
  Omd_representation.ref_container ->
  Omd_representation.element list ->
  'a ->
  Omd_representation.tok list ->
  (Omd_representation.element list * Omd_representation.tok list *
   Omd_representation.tok list)
  option
val maybe_link :
  ('a list -> 'b list -> Omd_representation.tok list -> Omd_representation.t) ->
  Omd_representation.element list ->
  'c ->
  Omd_representation.tok list ->
  (Omd_representation.element list * Omd_representation.tok list *
   Omd_representation.tok list)
  option
val bcode :
  Omd_representation.element list ->
  'a ->
  Omd_representation.tok list ->
  Omd_representation.element list * Omd_representation.tok list *
  Omd_representation.tok list
val icode :
  Omd_representation.element list ->
  'a ->
  Omd_representation.tok list ->
  Omd_representation.element list * Omd_representation.tok list *
  Omd_representation.tok list
val parse_list :
  ('a list ->
   Omd_representation.tok list ->
   Omd_representation.tok list -> Omd_representation.t) ->
  'b ->
  Omd_representation.element list ->
  'c ->
  Omd_representation.tok list ->
  Omd_representation.element list * Omd_representation.tok list *
  Omd_representation.tok list

val spaces :
  ('a list ->
   Omd_representation.tok list ->
   Omd_representation.tok list -> Omd_representation.t) ->
  'b ->
  int ->
  Omd_representation.element list ->
  Omd_representation.tok list ->
  Omd_representation.tok list ->
  Omd_representation.element list * Omd_representation.tok list *
  Omd_representation.tok list
(** spaces: returns (r,p,l) where r is the result, p is the last thing
      read, l is the remains *)

val main_parse :
  (Omd_representation.t ->
   Omd_representation.tok list ->
   Omd_representation.tok list ->
   (Omd_representation.t * Omd_representation.tok list *
    Omd_representation.tok list)
   option)
  list -> Omd_representation.tok list -> Omd_representation.t
val parse :
  ?extensions:(Omd_representation.t ->
               Omd_representation.tok list ->
               Omd_representation.tok list ->
               (Omd_representation.t * Omd_representation.tok list *
                Omd_representation.tok list)
               option)
              list ->
  Omd_representation.tok list -> Omd_representation.t

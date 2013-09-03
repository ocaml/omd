(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

type r = Omd_representation.t
(** accumulator (beware, reversed tokens) *)

and p = Omd_representation.tok list
(** context information: previous elements *)

and l = Omd_representation.tok list
(** tokens to parse *)

and main_loop =
  r -> (* accumulator (beware, reversed tokens) *)
  p -> (* info: previous elements *)
  l -> (* tokens to parse *)
  Omd_representation.t (* final result *)
(** most important loop, which has to be given as an argument *)

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

val tag__maybe_h1 : main_loop -> Omd_representation.tok
(** [tag__maybe_h1] is a tag that is injected everywhere that might preceed
    a H1 title. It needs [main_loop] as argument because when it builds
    a...................... *)

val tag__maybe_h2 : main_loop -> Omd_representation.tok

val tag__md : Omd_representation.t -> Omd_representation.tok
(** [tag__md] is basically a sort of [t_to_tok] function as its type
    tells. Its purpose is to inject pre-parsed markdown in a
    yet-to-parse token stream. *)

val tag_setext :
  main_loop -> Omd_representation.tok list -> Omd_representation.tok list
(** Let's tag the lines that *might* be titles using setext-style.
    "might" because if they are, for instance, in a code section,
    then they are not titles at all. *)


val hr_m : l -> l option
(** [hr_m l] returns [Some nl] where [nl] is the remaining of [l] if [l]
    contains a horizontal rule drawn with dashes. If it doesn't, then
    returns [None].*)

val hr_s : l -> l option
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


val read_title : main_loop -> int -> r -> p -> l -> r * p * l

val maybe_extension :
  (r -> p -> l -> (r * p * l) option) list ->
  r -> p -> l -> (r * p * l) option
(** [maybe_extension e r p l] returns [None] if there is no extension or
    if extensions haven't had  any effect, returns [Some(nr, np, nl)] if
    at least one extension has applied successfully. *)

val emailstyle_quoting : main_loop -> r -> p -> l -> r * p * l

val maybe_reference :
  Omd_representation.ref_container -> r -> p -> l -> (r * p * l) option

val maybe_link : main_loop -> r -> p -> l -> (r * p * l) option

val bcode : r -> p -> l -> r * p * l

val icode : r -> p -> l -> r * p * l

val parse_list : main_loop -> r -> p -> l -> r * p * l

val spaces : main_loop -> int -> r -> p -> l -> r * p * l
(** spaces: returns (r,p,l) where r is the result, p is the last thing
      read, l is the remains *)

val parse : ?extensions:(r -> p -> l -> (r * p * l) option) list -> l
  -> Omd_representation.t

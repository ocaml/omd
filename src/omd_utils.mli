val debug : bool
val trackfix : bool
val raise : exn -> 'a
module StringSet :
  sig
    type elt = string
    type t
    val empty : t
    val add : elt -> t -> t
    val mem : elt -> t -> bool
    val union : t -> t -> t
    val of_list : elt list -> t
  end
type 'a split = 'a list -> 'a split_action
and 'a split_action =
    Continue
  | Continue_with of 'a list * 'a list
  | Split of 'a list * 'a list
val fsplit_rev :
  ?excl:('a list -> bool) ->
  f:'a split -> 'a list -> ('a list * 'a list) option
(** [fsplit_rev ?excl ~f l] returns [Some(x,y)] where [x] is the
    **reversed** list of the consecutive elements of [l] that obey the
    split function [f].
    Note that [f] is applied to a list of elements and not just an
    element, so that [f] can look farther in the list when applied.
    [f l] returns [Continue] if there're more elements to consume,
    [Continue_with(left,right)] if there's more elements to consume
    but we want to choose what goes to the left part and what remains
    to process (right part), and returns [Split(left,right)] if
    the splitting is decided.
    When [f] is applied to an empty list, if it returns [Continue]
    then the result will be [None].

    If [excl] is given, then [excl] is applied before [f] is, to check
    if the splitting should be stopped right away. When the split
    fails, it returns [None]. *)

val fsplit :
  ?excl:('a list -> bool) ->
  f:'a split -> 'a list -> ('a list * 'a list) option
(** [fsplit ?excl ~f l] returns [Some(List.rev x, y)] if [fsplit ?excl
    ~f l] returns [Some(x,y)], else it returns [None]. *)


val id_of_string : < mangle : string -> string; .. > -> string -> string
(** [id_of_string ids id] returns a mangled version of [id], using the
    method [ids#mangle]. If you don't need mangling, you may use
    [object method mangle x = x end] for [ids].  However, the name
    [ids] also means that your object should have knowledge of all IDs
    it has issued, in order to avoid collision. This is why
    [id_of_string] asks for an object rather than "just a
    function". *)

val htmlentities : ?md:bool -> string -> string
(** [htmlentities s] returns a new string in which html-significant
    characters have been converted to html entities. For instance,
    "<Foo&Bar>" is converted to "&lt;Foo&amp;Bar&gt;". *)

val minimalize_blanks : string -> string
(** [minimalize_blanks s] returns a copy of [s] in which the first and last
   characters are never blank, and two consecutive blanks never happen. *)


val eat : ('a -> bool) -> 'a list -> 'a list
(** [eat f l] returns [l] where elements satisfying [f] have been removed,
    but it stops removing as soon as one element doesn't satisfy [f]. *)

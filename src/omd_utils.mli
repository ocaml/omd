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
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end
type 'a split = 'a list -> 'a split_action
and 'a split_action =
    Continue
  | Continue_with of 'a list * 'a list
  | Split of 'a list * 'a list
val fsplit_rev :
  ?excl:('a list -> bool) ->
  f:'a split -> 'a list -> ('a list * 'a list) option
val fsplit :
  ?excl:('a list -> bool) ->
  f:'a split -> 'a list -> ('a list * 'a list) option

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


val extract_html_attributes : string -> (string * string) list
(** Takes some HTML and returns the list of attributes of the first
    HTML tag.
    Notes:
    * Doesn't check the validity of HTML tags or attributes.
    * Doesn't support backslash escaping.
    * Attribute names are delimited by the space and equal characters.
    * Attribute values are either delimited by the double quote
      or the simple quote character.
*)

val extract_inner_html : string -> string
(** Takes an HTML node and returns the contents of the node.
    If it's not given a node, it returns something rubbish.
*)

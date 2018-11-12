(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013/2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

val id_of_string : < mangle : string -> string; .. > -> string -> string
(** [id_of_string ids id] returns a mangled version of [id], using the
    method [ids#mangle]. If you don't need mangling, you may use
    [object method mangle x = x end] for [ids].  However, the name
    [ids] also means that your object should have knowledge of all IDs
    it has issued, in order to avoid collision. This is why
    [id_of_string] asks for an object rather than "just a
    function". *)

val htmlentities: string -> string
(** [htmlentities s] returns a new string in which html-significant
    characters have been converted to html entities. For instance,
    "<Foo&Bar>" is converted to "&lt;Foo&amp;Bar&gt;". *)

(** Convert values of type {!type:Omd.doc} to values of type {!type:Tyxml.Html.doc} *)

(** [of_doc doc] is a {{:https://ocsigen.org/tyxml} Tyxml} document
    representation markdown data [doc] as statically validated
    {{:https://ocsigen.org/tyxml/latest/api/Html_sigs.T#TYPEdoc} HTML  document}. *)
val of_doc : ?title:string -> Omd.doc -> Tyxml.Html.doc

(** [of_fragment omd] is a {{:https://ocsigen.org/tyxml} Tyxml} representation
    of the
    {{:https://www.w3.org/TR/2011/WD-html5-20110525/content-models.html#flow-content}
    flow} elements corresponding to the a given [omd]. This is useful when the
    given [omd] is a fragment rather than a standalone document. *)
val of_fragment : Omd.doc -> Html_types.flow5 Tyxml.Html.elt list

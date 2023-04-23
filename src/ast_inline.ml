(* TODO The presence of `attrs` in several of these nodes is leaking an
        implementation detail: we have no support for attributes in `Concat`
        `Soft_break` or `Html` nodes. The attributes are just dropped during
        rendering.  Should we remove this from the UI, or should we include
        those somehow? Or should we include these in the document model, but
        but with the caveat that most renderings of the document don't support
        attributes in these nodes? *)
type 'attr inline =
  | Concat of 'attr * 'attr inline list
  | Text of 'attr * string
  | Emph of 'attr * 'attr inline
  | Strong of 'attr * 'attr inline
  | Code of 'attr * string
  | Hard_break of 'attr
  | Soft_break of 'attr
  | Link of 'attr * 'attr link
  | Image of 'attr * 'attr link
  | Html of 'attr * string

and 'attr link =
  { label : 'attr inline
  ; destination : string
  ; title : string option
  }

let rec of_cst_inline (cst : 'attr Cst_inline.inline) : 'attr inline =
  match cst with
  | Cst_inline.Strong (attr, _, inline) -> Strong (attr, of_cst_inline inline)
  | Cst_inline.Concat (attr, inline) ->
      Concat (attr, inline |> List.map of_cst_inline)
  | Cst_inline.Text (attr, s) -> Text (attr, s)
  | Cst_inline.Emph (attr, inline) -> Emph (attr, of_cst_inline inline)
  | Cst_inline.Code (attr, s) -> Code (attr, s)
  | Cst_inline.Hard_break attr -> Hard_break attr
  | Cst_inline.Soft_break attr -> Soft_break attr
  | Cst_inline.Link (attr, { label; destination; title }) ->
      Link (attr, { label = of_cst_inline label; destination; title })
  | Cst_inline.Image (attr, { label; destination; title }) ->
      Image (attr, { label = of_cst_inline label; destination; title })
  | Cst_inline.Html (attr, s) -> Html (attr, s)

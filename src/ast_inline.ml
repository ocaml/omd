(* TODO The presence of `attrs` in several of these nodes is leaking an
        implementation detail: we have no support for attributes in `Concat`
        `Soft_break` or `Html` nodes. The attributes are just dropped during
        rendering.  Should we remove this from the UI, or should we include
        those somehow? Or should we include these in the document model, but
        but with the caveat that most renderings of the document don't support
        attributes in these nodes? *)
type 'attr inline =
  | Empty
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

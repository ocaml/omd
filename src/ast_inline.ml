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

let remove_escape_chars (s : string) : string =
  let is_punct = function
    | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
    | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\'
    | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' ->
        true
    | _ -> false
  in
  let n = String.length s in
  let buf = Buffer.create n in
  let rec loop i =
    if i >= n then Buffer.contents buf
    else if s.[i] = '\\' && i + 1 < n && is_punct s.[i + 1] then (
      Buffer.add_char buf s.[i + 1];
      loop (i + 2))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

let rec of_cst_inline ?(escape = true) (cst : 'attr Cst_inline.inline) :
    'attr inline =
  match cst with
  | Cst_inline.Strong (attr, _, inline) -> Strong (attr, of_cst_inline inline)
  | Cst_inline.Concat (attr, inline) ->
      Concat (attr, inline |> List.map of_cst_inline)
  | Cst_inline.Text (attr, s) ->
      Text (attr, if escape then remove_escape_chars s else s)
  | Cst_inline.Emph (attr, _, inline) -> Emph (attr, of_cst_inline inline)
  | Cst_inline.Code (attr, s) -> Code (attr, s)
  | Cst_inline.Hard_break attr -> Hard_break attr
  | Cst_inline.Soft_break attr -> Soft_break attr
  | Cst_inline.Link (attr, link_type, { label; destination; title }) ->
      Link
        ( attr
        , { label = of_cst_inline ~escape:(link_type = Regular) label
          ; destination
          ; title
          } )
  | Cst_inline.Image (attr, { label; destination; title }) ->
      Image (attr, { label = of_cst_inline label; destination; title })
  | Cst_inline.Html (attr, s) -> Html (attr, s)

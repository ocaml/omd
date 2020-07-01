open Tyxml

let of_attributes _ = []

let rec of_inline ({il_attributes; il_desc} : Omd.inline) =
  let a = of_attributes il_attributes in
  match il_desc with
  | Code _     -> raise (Failure "TODO of_inline Code")
  | Concat ls  -> List.concat_map of_inline ls
  | Emph e     -> [Html.em ~a (of_inline e)]
  | Strong s   -> [Html.strong ~a (of_inline s)]
  | Hard_break -> raise (Failure "TODO of_inline Hard_break")
  (* TODO Add option for verified html ?*)
  | Html raw   -> [Tyxml.Html.Unsafe.data raw]
  | Image _    -> raise (Failure "TODO of_inline Image")
  | Link _     -> raise (Failure "TODO of_inline Link")
  | Soft_break -> raise (Failure "TODO of_inline Soft_break")
  | Text t     -> [Html.txt t]

let of_block (block : Omd.block) =
  match block.bl_desc with
  | Paragraph inline  -> Html.p (of_inline inline)
  | List _            -> raise (Failure "TODO of_block")
  | Blockquote _      -> raise (Failure "TODO of_block")
  | Thematic_break    -> raise (Failure "TODO of_block")
  | Heading _         -> raise (Failure "TODO of_block")
  | Code_block _      -> raise (Failure "TODO of_block")
  | Html_block _      -> raise (Failure "TODO of_block")
  | Link_def _        -> raise (Failure "TODO of_block")
  | Definition_list _ -> raise (Failure "TODO of_block")

let of_omd ?(title="") : Omd.doc -> Tyxml.Html.doc =
  fun omd ->
  let title' = title in
  let body' = List.map of_block omd in
  let open Html in
  html
    (head (title (txt title')) [])
    (body body')

(* module Html_importer = Xml_stream.signal *)
open Tyxml


(* let rec of_inline (inline : Omd.Inline.t) =
 *   match inline with
 *   | Concat ls  -> List.concat_map of_inline ls
 *   | Text t     -> [Html.txt t]
 *   | Emph e     -> [of_emph e]
 *   | Code _     -> raise (Failure "TODO of_inline Code")
 *   | Hard_break -> raise (Failure "TODO of_inline Hard_break")
 *   | Soft_break -> raise (Failure "TODO of_inline Soft_break")
 *   | Link _     -> raise (Failure "TODO of_inline Link")
 *   | Ref _      -> raise (Failure "TODO of_inline Ref")
 *   | Html raw   -> [Tyxml.Html.Unsafe.data raw]
 *   | Tag _      -> raise (Failure "TODO of_inline tag")
 *
 * and of_emph (emph : Omd.Inline.emph) =
 *   match emph.kind with
 *   | Omd.Normal -> Html.em (of_inline emph.content)
 *   | Omd.Strong -> Html.strong (of_inline emph.content)
 *
 * let of_block (block : Omd.Block.t) =
 *   match block with
 *   | Paragraph inline -> Html.p (of_inline inline)
 *   | List _         -> raise (Failure "TODO of_block")
 *   | Blockquote _   -> raise (Failure "TODO of_block")
 *   | Thematic_break -> raise (Failure "TODO of_block")
 *   | Heading _      -> raise (Failure "TODO of_block")
 *   | Code_block _   -> raise (Failure "TODO of_block")
 *   | Html_block _   -> raise (Failure "TODO of_block")
 *   | Link_def _     -> raise (Failure "TODO of_block")
 *   | Def_list _     -> raise (Failure "TODO of_block")
 *   | Tag_block _    -> raise (Failure "TODO of_block") *)

let rec of_omd_html (h : Omd.Html.t) =
  match h with
  | Omd.Html.Element (eltype, tag, attrs, child) -> of_element eltype tag attrs child
  | Omd.Html.Text _ -> raise (Failure "TODO")
  | Omd.Html.Raw _ -> raise (Failure "TODO")
  | Omd.Html.Null -> raise (Failure "TODO")
  | Omd.Html.Concat (_, _) -> raise (Failure "TODO")

and of_element =
  fun element_type tag attrs child ->
  match element_type with
  | Inline -> of_inline tag attrs child
  | Block -> of_block tag attrs child

and of_inline _ _ _ = raise (Failure "TODO")
and of_block _ _ _ = raise (Failure "TODO")


let of_omd ?(title="") : Omd.doc -> Tyxml.Html.doc =
  fun omd ->
  let omd_html = Omd.Html.of_doc omd in
  let title' = title in
  let body' = of_omd_html omd_html in
  let open Html in
  html
    (head (title (txt title')) [])
    (body body')

open Tyxml

let of_code c = Html.[code ~a:[] [txt c]]

let of_attribute (attr, value) =
  Html.Unsafe.string_attrib attr value

let of_attributes attrs =
  List.map of_attribute attrs

exception Invalid_markdown of string

module Attrs = struct
  let of_link_def : 'a Omd.link_def -> [< Html_types.a_attrib] Html.attrib list =
    fun l ->
      let href = Omd.Internal.escape_uri l.destination |> Html.a_href in
      match l.title with
      | Some t -> [href; Html.a_title t]
      | None   -> [href]
end
(* let link_attribs _  *)
(* let common_attributes : Omd.attributes -> Html_types.common =
 *   fun ((attr, value) :: _) -> *)
(* raise (Failure "TODO") *)

(* let img_attributes : Omd.attributes -> [< Html_types.img_attrib] list =
 *   let img_attrib (attr, value) =
 *     match attr with
 *     | "alt" -> Html.a_alt value
 *     | _ -> raise (Failure "TODO img_attrib")
 *   in
 *   fun attrs -> List.map img_attrib attrs *)

let rec inline_to_plaintext : Omd.inline -> string =
  fun il ->
  match il.il_desc with
  (* FIXME nontail rec *)
  | Concat xs -> List.map inline_to_plaintext xs |> String.concat ""
  | Text s -> s
  | Emph t -> inline_to_plaintext t
  | Strong t -> inline_to_plaintext t
  | Code s -> s
  | Hard_break -> ""
  | Soft_break -> ""
  | Html s -> s
  | Link l | Image l -> inline_to_plaintext l.label

let rec of_inline ({il_attributes; il_desc} : Omd.inline) =
  (* let a = of_attributes il_attributes in *)
  (* TODO Attributes *)
  let _ = il_attributes in
  match il_desc with
  | Code c     -> of_code c
  | Concat ls  -> List.concat_map of_inline ls
  | Emph e     -> Html.[em ~a:[] (of_inline e)]
  | Strong s   -> Html.[strong ~a:[] (of_inline s)]
  | Hard_break -> Html.[br ~a:[] ()]
  (* TODO Add option for verified html ?*)
  | Html raw   -> Html.Unsafe.[data raw]
  | Image img  -> [of_img img]
  | Link l     -> Html.[a ~a:Attrs.(of_link_def l) (of_link_label l.label)]
  | Soft_break -> Html.[txt "\n"]
  | Text t     -> Html.[txt t]

and of_link_label ({il_attributes; il_desc} as il : Omd.inline) =
  let _attr = il_attributes in
  match il_desc with
  (* | Code c   -> of_code c
   * | Text t   -> Html.[txt t]
   * | Concat l -> List.concat_map of_link_label l
   * | Emph e   -> Html.[em ~a:[] (of_link_label e)]
   * | Strong e -> of_inline e |> List.map Html.Unsafe.coerce_elt
   * | Image i  -> [Html.Unsafe.coerce_elt (of_img i )] *)
  | Code _ | Text _   | Concat _
  | Emph _ | Strong _ | Image _ -> List.map Html.Unsafe.coerce_elt (of_inline il)
  | _        -> raise (Failure "TODO invalid")
(* | Emph _ -> (??)
 * | Strong _ -> (??)
 * | Hard_break -> (??)
 * | Soft_break -> (??)
 * | Link _ -> (??)
 * | Image _ -> (??)
 * | Html _ -> (??) *)

and of_img (img : Omd.inline Omd.link_def) =
  let escaped_url = Omd.Internal.escape_uri img.destination in
  let attrs = Option.map Html.a_title img.title |> Option.to_list in
  let alt = inline_to_plaintext img.label in
  Html.(img ~src:escaped_url ~alt ~a:attrs ())

let of_heading n inline =
  let h =
    let open Html in
    match n with
    | 1 -> h1
    | 2 -> h2
    | 3 -> h3
    | 4 -> h4
    | 5 -> h5
    | 6 -> h6
    | m -> raise (Invalid_markdown (Printf.sprintf "heading number %d" m))
  in
  h ~a:[] (of_inline inline)

let of_link_block (ld : string Omd.link_def) =
  Html.(p [a ~a:Attrs.(of_link_def ld) [txt ld.label]])

(* This function is partial because the Omd AST includes nodes which do not
   correspond to any HTML element. *)
let of_block : Omd.block -> _ Html.elt option =
  fun block ->
  (* FIXME *)
  try
    match block.bl_desc with
    | Paragraph i       -> Some (Html.p (of_inline i))
    | List _            -> raise (Failure "TODO of_block list")
    | Blockquote _      -> raise (Failure "TODO of_block blockquote")
    | Thematic_break    -> raise (Failure "TODO of_block Thematic_break")
    | Heading (n, i)    -> Some (of_heading n i)
    | Code_block (_, c) -> Some (Html.(pre [code ~a:[] [txt c]]))
    (* Html.pre ~a:[] (of_inline i) *)
    | Html_block _      -> raise (Failure "TODO of_block Html_bloc")
    | Link_def _        -> None
    | Definition_list _ -> raise (Failure "TODO of_block Definition_list")
  with (Failure err)    ->
    Some (Html.(h1 [txt  ("Error " ^ err)]))

let of_omd ?(title="") : Omd.doc -> Tyxml.Html.doc =
  fun omd ->
  let title' = title in
  let body' =
    try
      List.filter_map of_block omd
    with (Failure err) ->
      Html.[h1 [txt  ("Error " ^ err)]]
  in
  let open Html in
  html
    (head (title (txt title')) [])
    (body body')

open Tyxml

let of_code attrs c = Html.[code ~a:attrs [txt c]]

let of_attribute (attr, value) =
  Html.Unsafe.string_attrib attr value

let of_attributes attrs =
  List.map of_attribute attrs

exception Invalid_markdown of string

exception Unsupported_attribute of string

let of_omd_attributes attrs =
  List.map (fun (a, v) -> Html.Unsafe.string_attrib a v) attrs
(* module Attrs = struct
 * end *)

(* TODO move into Omd module *)
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
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Code c     -> of_code attrs c
  | Concat ls  -> List.concat_map of_inline ls
  | Emph e     -> Html.[em ~a:[] (of_inline e)]
  | Strong s   -> Html.[strong ~a:[] (of_inline s)]
  | Hard_break -> Html.[br ~a:[] ()]
  (* TODO Add option for verified html ?*)
  | Html raw   -> Html.Unsafe.[data raw]
  | Image img  -> [of_img attrs img]
  | Link l     -> [of_link attrs l]
  | Soft_break -> Html.[txt "\n"]
  | Text t     -> Html.[txt t]

and of_link_label ({il_attributes; il_desc} as il : Omd.inline) =
  let _attr = il_attributes in
  match il_desc with
  | Code _ | Text _   | Concat _
  | Emph _ | Strong _ | Image _ -> List.map Html.Unsafe.coerce_elt (of_inline il)
  | _        -> raise (Failure "TODO invalid link label")

and of_link attrs (l : Omd.link) =
  let escaped_url = Omd.Internal.escape_uri l.destination in
  let attrs = (Html.a_href escaped_url :: attrs) @ Option.to_list (Option.map Html.a_title l.title)  in
  Html.(a ~a:attrs (of_link_label l.label))

and of_img attrs (img : Omd.link) =
  let escaped_url = Omd.Internal.escape_uri img.destination in
  let attrs = attrs @ (Option.map Html.a_title img.title |> Option.to_list) in
  let alt = inline_to_plaintext img.label in
  Html.(img ~src:escaped_url ~alt ~a:attrs ())

let of_heading n attrs content =
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
  h ~a:(of_omd_attributes attrs) (of_inline content)

let of_code_block src attrs content =
  let src_attr = match src with
    | "" -> []
    | _  -> [Html.a_class ["language-" ^ src]]
  in
  Html.(pre ~a:attrs [code ~a:src_attr [txt content]])

(* This function is partial because the Omd AST includes nodes which do not
   correspond to any HTML element. *)
let rec of_block : Omd.block -> _ Html.elt =
  fun block ->
  (* FIXME *)
  let attrs = of_omd_attributes block.bl_attributes in
  try
    match block.bl_desc with
    | Paragraph i                -> Html.p (of_inline i)
    | List (typ, spacing, items) -> of_list typ spacing items
    | Blockquote content         -> Html.blockquote (List.map of_block content)
    | Thematic_break             -> Html.hr ()
    | Heading (n, content)       -> of_heading n block.bl_attributes content
    | Code_block (src, c)        -> of_code_block src attrs c
    | Html_block html            -> Html.Unsafe.data html
    | Definition_list content    -> of_definition_list content
  with (Failure err) ->
    Html.(h1 [txt  ("Error " ^ err)])

and of_list typ spacing items =
  let of_list_block (bl : Omd.block) =
    match bl.bl_desc, spacing with
    | Paragraph il, Tight -> of_inline il |> List.map Html.Unsafe.coerce_elt
    | _ -> [of_block bl]
  in
  let itemize i =
    i |> List.concat_map of_list_block |> Html.li
  in
  let element =
    match typ with
    | Ordered (start, _) -> Html.ol ~a:(if start <> 1 then [Html.a_start start] else [])
    | Bullet _           -> Html.ul ~a:[]
  in
  items
  |> List.map itemize
  |> element

and of_definition_list defs =
  let definiens d =
    Html.dd (of_inline d |> List.map Html.Unsafe.coerce_elt)
  in
  let def ({term; defs} : Omd.def_elt) =
    Html.(dt (of_inline term |> List.map Html.Unsafe.coerce_elt)
          :: List.map definiens defs)
  in
  Html.dl (List.concat_map def defs)

let of_omd ?(title="") : Omd.doc -> Tyxml.Html.doc =
  fun omd ->
  let title' = title in
  let body' =
    try
      List.map of_block omd
    with (Failure err) ->
      Html.[h1 [txt  ("Error " ^ err)]]
  in
  let open Html in
  html
    (head (title (txt title')) [])
    (body body')

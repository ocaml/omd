open Tyxml

let of_code c = Html.[code ~a:[] [txt c]]

let of_attribute (attr, value) =
  Html.Unsafe.string_attrib attr value

let of_attributes attrs =
  List.map of_attribute attrs

(* let common_attributes : Omd.attributes -> Html_types.common =
 *   fun ((attr, value) :: _) -> *)
(* raise (Failure "TODO") *)

(* let image_attributes : Omd.attributes -> Html_types.img_attrib list =
 *   fun _ -> raise (Failure "TODO") *)

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
  | Image img  -> of_image img
  | Link l     -> of_inline_link l
  | Soft_break -> Html.[txt "\n"]
  | Text t     -> Html.[txt t]

and of_link_label ({il_attributes; il_desc} : Omd.inline) =
  let _attr = il_attributes in
  match il_desc with
  | Code c -> of_code c
  | Text t -> Html.[txt t]
  | Concat l -> List.concat_map of_link_label l
  | Emph e -> Html.[em ~a:[] (of_link_label e)]
  (* | Image _ -> (??)           *)
  | _ -> raise (Failure "TODO invalid")
(* | Emph _ -> (??)
 * | Strong _ -> (??)
 * | Hard_break -> (??)
 * | Soft_break -> (??)
 * | Link _ -> (??)
 * | Image _ -> (??)
 * | Html _ -> (??) *)

and of_image (img : Omd.inline Omd.link_def) =
  let escaped_url = Omd.Internal.escape_uri img.destination in
  let title' = Option.value ~default:"" img.title in
  let alt = "TODO" in
  (* let attrs = img in *)
  Html.[img ~src:escaped_url ~alt ~a:[a_title title'] ()]

and of_inline_link  =
  fun (l : Omd.inline Omd.link_def) ->
  let escaped_url = Omd.Internal.escape_uri l.destination in
  Html.[a ~a:[a_href escaped_url] (of_link_label l.label)]


exception Invalid_markdown of string

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


let of_block (block : Omd.block) =
  (* FIXME *)
  try
    match block.bl_desc with
    | Paragraph i       -> Html.p (of_inline i)
    | List _            -> raise (Failure "TODO of_block")
    | Blockquote _      -> raise (Failure "TODO of_block")
    | Thematic_break    -> raise (Failure "TODO of_block")
    | Heading (n, i)    -> of_heading n i
    | Code_block (_, _) -> raise (Failure "TODO of Code_block")
    (* Html.pre ~a:[] (of_inline i) *)
    | Html_block _      -> raise (Failure "TODO of_block")
    | Link_def _        -> raise (Failure "TODO of_block")
    | Definition_list _ -> raise (Failure "TODO of_block")
  with (Failure err)    ->
    Html.(h1 [txt  ("Error " ^ err)])

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

open Tyxml

(* TODO Document *)
(* TODO Fix tests *)
(* TODO self-review and cleanup *)

let cons_opt x_opt xs =
  match x_opt with
  | None -> xs
  | Some x -> x :: xs

exception Invalid_markdown of string

exception Unsupported_attribute of string

let of_omd_attributes attrs =
  List.map (fun (a, v) -> Html.Unsafe.string_attrib a v) attrs


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


let of_code attrs c = Html.[code ~a:attrs [txt c]]

(* NOTE: The unfortunate duplication of inline handlers seems to be necessary
   to get the Tyxml types constructed formed correctly. *)
(* TODO Support verified html (instead of using Html.Unsafe.data) ?*)
let rec of_inline ({il_attributes; il_desc} : Omd.inline) : Html_types.phrasing Html.elt list =
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Code c     -> of_code attrs c
  | Emph e     -> Html.[em ~a:[] (of_inline e)]
  | Strong s   -> Html.[strong ~a:[] (of_inline s)]
  | Hard_break -> Html.[br ~a:[] ()]
  | Html raw   -> Html.Unsafe.[data raw]
  | Soft_break -> Html.[txt "\n"]
  | Text t     -> Html.[txt t]
  | Concat ls  -> List.concat_map of_inline ls
  | Link l     -> [of_link attrs l]
  | Image img  -> [(of_img attrs img :> Html_types.phrasing Html.elt)]

and of_list_item_content ({il_desc; il_attributes} : Omd.inline) : Html_types.li_content_fun Html.elt list =
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Code c     -> of_code attrs c
  | Emph e     -> Html.[em ~a:[] (of_inline e)]
  | Strong s   -> Html.[strong ~a:[] (of_inline s)]
  | Hard_break -> Html.[br ~a:[] ()]
  | Html raw   -> Html.Unsafe.[data raw]
  | Soft_break -> Html.[txt "\n"]
  | Text t     -> Html.[txt t]
  | Concat ls  -> List.concat_map of_list_item_content ls
  | Link l     -> [(of_link attrs l :> Html_types.li_content_fun Html.elt)]
  | Image img  -> [(of_img attrs img :> Html_types.li_content_fun Html.elt)]

and of_def_term ({il_desc; il_attributes} : Omd.inline) : Html_types.dt_content Html.elt list =
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Code c     -> of_code attrs c
  | Emph e     -> Html.[em ~a:[] (of_inline e)]
  | Strong s   -> Html.[strong ~a:[] (of_inline s)]
  | Hard_break -> Html.[br ~a:[] ()]
  | Html raw   -> Html.Unsafe.[data raw]
  | Soft_break -> Html.[txt "\n"]
  | Text t     -> Html.[txt t]
  | Concat ls  -> List.concat_map of_def_term ls
  | Link l     -> [(of_link attrs l :> Html_types.dt_content Html.elt)]
  | Image img  -> [(of_img attrs img :> Html_types.dt_content Html.elt)]

and of_link_label ({il_desc; il_attributes} : Omd.inline) =
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Text t     -> Html.[txt t]
  | Code c     -> of_code attrs c
  | Concat ls  -> List.concat_map of_link_label ls
  | Emph e     -> Html.[em ~a:[] (of_link_label e)]
  | Strong s   -> Html.[strong ~a:[] (of_link_label s)]
  | Image img  -> [(of_img attrs img :> Html_types.phrasing_without_interactive Html.elt)]
  | _          -> []

and of_link attrs (l : Omd.link) =
  let escaped_url = Omd.Internal.escape_uri l.destination in
  let attrs =
    cons_opt (Option.map Html.a_title l.title) (Html.a_href escaped_url :: attrs)
  in
  Html.(a ~a:attrs (of_link_label l.label))

and of_img attrs (img : Omd.link) =
  let escaped_url = Omd.Internal.escape_uri img.destination in
  let attrs = cons_opt (Option.map Html.a_title img.title) attrs in
  let alt = inline_to_plaintext img.label in
  Html.(img ~src:escaped_url ~alt ~a:attrs ())

let of_heading n attrs content =
  let ctr =
    let open Html in
    match n with
    | 1 -> h1
    | 2 -> h2
    | 3 -> h3
    | 4 -> h4
    | 5 -> h5
    | 6 -> h6
    | _ -> p  (* See ATX Headings in the tests/spec.txt *)
  in
  ctr ~a:attrs (of_inline content)

let of_code_block src attrs content =
  let src_attr = match src with
    | "" -> []
    | _  -> [Html.a_class ["language-" ^ src]]
  in
  Html.(pre ~a:attrs [code ~a:src_attr [txt content]])

let rec of_block : Omd.block -> _ Html.elt =
  fun block ->
  let attrs = of_omd_attributes block.bl_attributes in
  match block.bl_desc with
  | Paragraph content          -> Html.p (of_inline content)
  | List (typ, spacing, items) -> of_list typ spacing items
  | Blockquote content         -> Html.blockquote (List.map of_block content)
  | Thematic_break             -> Html.hr ()
  | Heading (n, content)       -> of_heading n attrs content
  | Code_block (src, code)     -> of_code_block src attrs code
  | Html_block html            -> Html.Unsafe.data html
  | Definition_list content    -> of_definition_list content

and of_list typ spacing items =
  let of_list_block (bl : Omd.block) : Html_types.li_content Html.elt list =
    match bl.bl_desc, spacing with
    | Paragraph il, Tight -> of_list_item_content il
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
  (* "The word or phrase that defines the definiendum in a definition." *)
  let definiens d =
    Html.dd (of_list_item_content d)
  in
  (* "The term—word or phrase—defined in a definition." *)
  let definiendum ({term; defs} : Omd.def_elt) =
    let definientia : Html_types.dl_content Html.elt list = List.map definiens defs in
    Html.(dt (of_def_term term))
    :: definientia
  in
  Html.dl (List.concat_map definiendum defs)

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

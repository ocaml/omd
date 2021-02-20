open Tyxml

(* TODO Document *)

(** TODO move into Omd if we don't replace the html module with this one *)

(** [cons_opt opt_x xs] is (x :: xs) if [opt_x] is [Some x] or else just [xs].*)
let cons_opt : 'a option -> 'a list -> 'a list =
  fun x_opt xs ->
  match x_opt with
  | None -> xs
  | Some x -> x :: xs

(** TODO move into Omd if we don't replace the html module with this one *)

(** [inline_to_plain_text il] is a string with just the textual content
    of the the inline term [il]. All semantic and formatting nodes are ignored.

    This is intended for use internally, for converting inline elements which
    do not support any markup, such as image labels. *)
let inline_to_plain_text : Omd.inline -> string =
  fun il ->
  let buf = Buffer.create 1024 in
  let rec go {Omd.il_desc; _} = match il_desc with
    | Concat xs -> List.iter go xs
    | Emph t | Strong t -> go t
    | Link l | Image l -> go l.label
    | Hard_break | Soft_break -> ()
    | Code s | Html s | Text s -> Buffer.add_string buf s
  in
  go il;
  Buffer.contents buf

let of_omd_attributes attrs =
  List.map (fun (a, v) -> Html.Unsafe.string_attrib a v) attrs


(* INLINE CONVERSION *)

(* NOTE: The unfortunate duplication of inline handlers seems to be necessary
   to get the Tyxml types constructed correctly. However, if you know how to
   simplify, please help! *)
(* TODO Support verified html (instead of using Html.Unsafe.data) ?*)
let rec of_inline : Omd.inline -> Html_types.phrasing Html.elt list =
  fun {il_attributes; il_desc} ->
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Html raw   -> Html.Unsafe.[data raw]
  | Code c     -> Html.[code ~a:attrs [txt c]]
  | Emph e     -> Html.[em ~a:attrs (of_inline e)]
  | Hard_break -> Html.[br ~a:attrs ()]
  | Soft_break -> Html.[txt "\n"]
  | Strong s   -> Html.[strong ~a:attrs (of_inline s)]
  | Text t     -> Html.[txt t]
  | Concat ls  -> List.concat_map of_inline ls
  | Link l     -> [of_link attrs l]
  | Image img  -> [(of_img attrs img :> Html_types.phrasing Html.elt)]

and of_def_term : Omd.inline -> Html_types.dt_content Html.elt list =
  fun {il_desc; il_attributes} ->
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Html raw   -> Html.Unsafe.[data raw]
  | Code c     -> Html.[code ~a:attrs [txt c]]
  | Emph e     -> Html.[em ~a:attrs (of_inline e)]
  | Hard_break -> Html.[br ~a:attrs ()]
  | Soft_break -> Html.[txt "\n"]
  | Strong s   -> Html.[strong ~a:attrs (of_inline s)]
  | Text t     -> Html.[txt t]
  | Concat ls  -> List.concat_map of_def_term ls
  | Link l     -> [(of_link attrs l :> Html_types.dt_content Html.elt)]
  | Image img  -> [(of_img attrs img :> Html_types.dt_content Html.elt)]

and of_link_label : Omd.inline -> Html_types.phrasing_without_interactive Html.elt list =
  fun {il_desc; il_attributes} ->
  let attrs = of_omd_attributes il_attributes in
  match il_desc with
  | Code c     -> Html.[code ~a:attrs [txt c]]
  | Emph e     -> Html.[em ~a:attrs (of_link_label e)]
  | Strong s   -> Html.[strong ~a:attrs (of_link_label s)]
  | Text t     -> Html.[txt t]
  | Concat ls  -> List.concat_map of_link_label ls
  | Image img  -> [(of_img attrs img :> Html_types.phrasing_without_interactive Html.elt)]
  (* We ignore any elements that shouldn't be included in link labels. *)
  | _          -> []

and of_link attrs (l : Omd.link) =
  let escaped_url = Omd.Internal.escape_uri l.destination in
  let attrs =
    let url = Html.a_href escaped_url in
    let title = Option.map Html.a_title l.title in
    (* The url goes before the title to match the order in the spec.txt *)
    url :: cons_opt (title) attrs
  in
  Html.(a ~a:attrs (of_link_label l.label))

and of_img attrs (img : Omd.link) =
  let escaped_url = Omd.Internal.escape_uri img.destination in
  let attrs = cons_opt (Option.map Html.a_title img.title) attrs in
  let alt = inline_to_plain_text img.label in
  Html.(img ~src:escaped_url ~alt ~a:attrs ())


(* BLOCK CONVERSION *)

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

let rec of_list (typ : Omd.list_type) (spacing : Omd.list_spacing) items =
  let of_list_block (bl : Omd.block) : Html_types.li_content Html.elt list =
    match bl.bl_desc, spacing with
    | Paragraph il, Tight -> (of_def_term il :> Html_types.li_content_fun Html.elt list)
    | _ -> [of_block bl]
  in
  let to_list_item i = List.concat_map of_list_block i |> Html.li in
  let to_list_element =
    match typ with
    | Ordered (start, _) -> Html.ol ~a:(if start <> 1 then [Html.a_start start] else [])
    | Bullet _           -> Html.ul ~a:[]
  in
  items
  |> List.map to_list_item
  |> to_list_element

and of_definition_list defs =
  let entry ({term; defs} : Omd.def_elt) =
    (* "The term — word or phrase — defined in a definition." *)
    let definiendum = Html.dt (of_def_term term) in
    (* "The words or phrases that define the definiendum in a definition." *)
    let definientia = List.map (fun d -> Html.dd (of_def_term d)) defs in
    definiendum :: definientia
  in
  Html.dl (List.concat_map entry defs)

and of_block : Omd.block -> Html_types.flow5 Html.elt =
  fun block ->
  let attrs = of_omd_attributes block.bl_attributes in
  match block.bl_desc with
  | Paragraph content          -> Html.p (of_inline content)
  | Blockquote content         -> Html.blockquote (List.map of_block content)
  | Thematic_break             -> Html.hr ()
  | Html_block html            -> Html.Unsafe.data html
  | List (typ, spacing, items) -> of_list typ spacing items
  | Heading (n, content)       -> of_heading n attrs content
  | Code_block (src, code)     -> of_code_block src attrs code
  | Definition_list content    -> of_definition_list content

let of_fragment : Omd.doc -> Html_types.flow5 Html.elt list =
  fun omd -> List.map of_block omd

let of_doc ?(title="") : Omd.doc -> Tyxml.Html.doc =
  fun omd ->
  let title' = title in
  let body' = of_fragment omd in
  let open Html in
  html
    (head (title (txt title')) [])
    (body body')

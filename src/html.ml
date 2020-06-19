open Ast

type attribute =
  string * string

type element_type =
  | Inline
  | Block

type t =
  | Element of element_type * string * attribute list * t option
  | Text of string
  | Raw of string
  | Null
  | Concat of t * t

let elt etype name attrs childs =
  Element (etype, name, attrs, childs)

let text s = Text s

let raw s = Raw s

let concat t1 t2 =
  match t1, t2 with
  | Null, t | t, Null -> t
  | _ -> Concat (t1, t2)

let concat_map f l =
  List.fold_left (fun accu x -> concat accu (f x)) Null l

(* only convert when "necessary" *)
let htmlentities s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then
      Buffer.contents b
    else begin
      begin match s.[i] with
      | '"' ->
          Buffer.add_string b "&quot;"
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | c ->
          Buffer.add_char b c
      end;
      loop (succ i)
    end
  in
  loop 0

let add_attrs_to_buffer buf attrs =
  let f (k, v) = Printf.bprintf buf " %s=\"%s\"" k (htmlentities v) in
  List.iter f attrs

let rec add_to_buffer buf = function
  | Element (Block, name, attrs, None) ->
      Printf.bprintf buf "<%s%a />\n"
        name add_attrs_to_buffer attrs
  | Element (Block, name, attrs, Some c) ->
      Printf.bprintf buf "<%s%a>\n%a\n</%s>\n"
        name add_attrs_to_buffer attrs add_to_buffer c name
  | Element (Inline, name, attrs, None) ->
      Printf.bprintf buf "<%s%a />"
        name add_attrs_to_buffer attrs
  | Element (Inline, name, attrs, Some c) ->
      Printf.bprintf buf "<%s%a>%a</%s>"
        name add_attrs_to_buffer attrs add_to_buffer c name
  | Text s ->
      Buffer.add_string buf (htmlentities s)
  | Raw s ->
      Buffer.add_string buf s
  | Null ->
      ()
  | Concat (t1, t2) ->
      add_to_buffer buf t1;
      add_to_buffer buf t2

let _percent_encode s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '!' | '*' | '\'' | '(' | ')' | ';' | ':'
      | '@' | '=' | '+' | '$' | ',' | '/' | '?' | '%'
      | '#' | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' as c ->
          Buffer.add_char b c
      | '&' ->
          Buffer.add_string b "&amp;"
      | _ as c ->
          Printf.bprintf b "%%%2X" (Char.code c)
    ) s;
  Buffer.contents b

let text_of_inline ast =
  let b = Buffer.create 101 in
  Text.inline b ast;
  Buffer.contents b

let attr {Attributes.id; classes; attributes} =
  let a =
    if classes <> [] then
      ("class", String.concat " " classes) :: attributes
    else
      attributes
  in
  match id with
  | None -> a
  | Some s -> ("id", s) :: a

let rec url label destination title attrs =
  let attrs =
    match title with
    | None -> attrs
    | Some title -> ("title", title) :: attrs
  in
  let attrs = ("href", destination) :: attrs in
  elt Inline "a" attrs (Some (inline label))

and img label destination title attrs =
  let attrs =
    match title with
    | None -> attrs
    | Some title -> ("title", title) :: attrs
  in
  let attrs =
    ("src", destination) ::
    ("alt", text_of_inline label) :: attrs
  in
  elt Inline "img" attrs None

and inline = function
  | Inline.Concat l ->
      concat_map inline l
  | Text t ->
      text t
  | Emph {kind; style = _; content} ->
      let name =
        match kind with
        | Normal -> "em"
        | Strong -> "strong"
      in
      elt Inline name [] (Some (inline content))
  | Code {level = _; attributes; content} ->
      elt Inline "code" (attr attributes) (Some (text content))
  | Hard_break ->
      elt Inline "br" [] None
  | Soft_break ->
      text "\n"
  | Html body ->
      raw body
  | Link {kind = Url; def = {label; destination; title; attributes}; _} ->
      url label destination title (attr attributes)
  | Link {kind = Img; def = {label; destination; title; attributes}; _} ->
      img label destination title (attr attributes)
  | Ref {kind = Url; label; def = {destination; title; attributes; _}; _} ->
      url label destination title (attr attributes)
  | Ref {kind = Img; label; def = {destination; title; attributes; _}; _} ->
      img label destination title (attr attributes)
  | Tag {tag = _; attributes = _; content} ->
      inline content

let rec block = function
  | Block.Blockquote q ->
      elt Block "blockquote" []
        (Some (concat_map block q))
  | Paragraph md ->
      elt Block "p" [] (Some (inline md))
  | List {kind; style; blocks} ->
      let name =
        match kind with
        | Ordered _ -> "<ol>"
        | Unordered _ -> "<ul>"
      in
      let attrs =
        match kind with
        | Ordered (n, _) when n > 1 ->
            ["start", string_of_int n]
        | _ ->
            []
      in
      let li t =
        match t, style with
        | Block.Paragraph t, Tight ->
            inline t
        | Link_def _, _ ->
            Null
        | _ ->
            block t
      in
      let li t = elt Block "li" [] (Some (concat_map li t)) in
      elt Block name attrs (Some (concat_map li blocks))
  | Code_block {kind = _; other = _; label; attributes; code} ->
      let attrs =
        match label with
        | Some language ->
            ["class", "language-" ^ language]
        | None ->
            []
      in
      let c =
        match code with
        | None ->
            Null
        | Some c ->
            text c
      in
      elt Block "pre" (attr attributes)
        (Some (elt Block "code" attrs (Some c)))
  | Thematic_break ->
      elt Block "hr" [] None
  | Html_block body ->
      raw body
  | Heading {level; attributes; text} ->
      elt Block ("h" ^ string_of_int level) (attr attributes)
        (Some (inline text))
  | Def_list {content} ->
      let f {Block.term; defs} =
        concat
          (elt Block "dt" [] (Some (inline term)))
          (concat_map (fun s -> elt Block "dd" [] (Some (inline s))) defs)
      in
      elt Block "dl" [] (Some (concat_map f content))
  | Tag_block {tag = _; attributes = _; content} ->
      concat_map block content
  | Link_def _ ->
      Null

let of_doc doc =
  concat_map block doc

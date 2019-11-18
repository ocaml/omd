open Ast

type t =
  | Atom of string
  | List of t list

let atom s = Atom s

let rec link_def : 'a. ('a -> t) -> 'a Link_def.t -> t =
  fun f {label; destination; title; _} ->
    let title = match title with Some title -> [Atom title] | None -> [] in
    List (Atom "link-def" :: f label :: Atom destination :: title)

and attributes = function
    { Attributes.id; classes; attributes } ->
      List begin
        begin match id with
        | None -> []
        | Some id -> [ Atom "id" ; Atom id ]
        end
        @
        [ Atom "classes"; List (List.map (fun s -> Atom s) classes)
        ; Atom "attributes"
        ; List (List.map (fun (k,v) -> List [Atom k; Atom v]) attributes)
        ]
      end

and inline = function
  | Concat xs ->
      List (Atom "concat" :: List.map inline xs)
  | Text s ->
      Atom s
  | Emph e ->
      List [Atom "emph"; inline e.content]
  | Code _ ->
      Atom "code"
  | Hard_break ->
      Atom "hard-break"
  | Soft_break ->
      Atom "soft-break"
  | Link {kind = Url; def; _} ->
      List [Atom "url"; link_def inline def]
  | Ref {kind = Url; label; def; _} ->
      List [Atom "url-ref"; inline label; link_def atom def]
  | Html s ->
      List [Atom "html"; Atom s]
  | Link {kind = Img; _} ->
      Atom "img"
  | Ref {kind = Img; label; def; _} ->
      List [Atom "img-ref"; inline label; link_def atom def]
  | Tag {tag; content; attributes=attr} -> List
        [ Atom "tag"; Atom tag
        ; Atom "attributes"; attributes attr
        ; Atom "content"; inline content
        ]

let rec block = function
  | Paragraph x ->
      List [Atom "paragraph"; inline x]
  | List l ->
      List (Atom "list" :: List.map (fun xs -> List (Atom "list-item" :: List.map block xs)) l.blocks)
  | Blockquote xs ->
      List (Atom "blockquote" :: List.map block xs)
  | Thematic_break ->
      Atom "thematic-break"
  | Heading {level; text; _} ->
      List [Atom "heading"; Atom (string_of_int level); inline text]
  | Code_block {kind = None; code = Some s; _} ->
      List [Atom "indented-code"; Atom s]
  | Code_block {kind = None; code = None; _} ->
      List [Atom "indented-code"]
  | Code_block {kind = Some _; code = Some s; _} ->
      List [Atom "fenced-code"; Atom s]
  | Code_block {kind = Some _; code = None; _} ->
      List [Atom "fenced-code"]
  | Html_block s ->
      List [Atom "html"; Atom s]
  | Link_def {label; destination; _} ->
      List [Atom "link-def"; Atom label; Atom destination]
  | Def_list {content} ->
      List [Atom "def-list"; List (List.map (fun elt -> List [inline elt.Def_list.term; List (List.map inline elt.defs)]) content)]
  | Tag_block {tag; content; attributes=attr} -> List
        [ Atom "tag"; Atom tag
        ; Atom "attributes"; attributes attr
        ; Atom "content"; List (List.rev_map block content)
        ]

let create ast =
  List (List.map block ast)

let needs_quotes s =
  let rec loop i =
    if i >= String.length s then
      false
    else begin
      match s.[i] with
      | ' ' | '\t' | '\x00'..'\x1F' | '\x7F'..'\x9F' ->
          true
      | _ ->
          loop (succ i)
    end
  in
  loop 0

let rec print ppf = function
  | Atom s when needs_quotes s ->
      Format.fprintf ppf "%S" s
  | Atom s ->
      Format.pp_print_string ppf s
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]" (Format.pp_print_list ~pp_sep:Format.pp_print_space print) l

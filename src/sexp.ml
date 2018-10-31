open Ast

type t =
  | Atom of string
  | List of t list

let rec link_def {label; destination; title} =
  let title = match title with Some title -> [Atom title] | None -> [] in
  List (Atom "link-def" :: inline label :: Atom destination :: title)

and inline = function
  | Concat xs ->
      List (Atom "concat" :: List.map inline xs)
  | Text s ->
      List [Atom "text"; Atom s]
  | Emph (_, _, x) ->
      List [Atom "emph"; inline x]
  | Code _ ->
      Atom "code"
  | Hard_break ->
      Atom "hard-break"
  | Soft_break ->
      Atom "soft-break"
  | Url def ->
      List [Atom "url"; link_def def]
  | Url_ref (label, def) ->
      List [Atom "url-ref"; inline label; link_def def]
  | Html s ->
      List [Atom "html"; Atom s]
  | Img _ ->
      Atom "img"
  | Img_ref (label, def) ->
      List [Atom "img-ref"; inline label; link_def def]

let rec block = function
  | Paragraph x ->
      List [Atom "paragraph"; inline x]
  | List (_, _, xs) ->
      List (Atom "list" :: List.map (fun xs -> List (Atom "list-item" :: List.map block xs)) xs)
  | Blockquote xs ->
      List (Atom "blockquote" :: List.map block xs)
  | Thematic_break ->
      Atom "thematic-break"
  | Heading (n, x) ->
      List [Atom "heading"; Atom (string_of_int n); inline x]
  | Code_block _ ->
      Atom "code"
  | Html_block s ->
      List [Atom "html"; Atom s]
  | Link_def _ ->
      List [Atom "link-def"]

let rec print ppf = function
  | Atom s ->
      Format.pp_print_string ppf s
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]" (Format.pp_print_list ~pp_sep:Format.pp_print_space print) l

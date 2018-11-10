open Ast

type t =
  | Atom of string
  | List of t list

let atom s = Atom s

let rec link_def : 'a. ('a -> t) -> 'a link_def -> t =
  fun f {label; destination; title} ->
    let title = match title with Some title -> [Atom title] | None -> [] in
    List (Atom "link-def" :: f label :: Atom destination :: title)

and inline = function
  | Concat xs ->
      List (Atom "concat" :: List.map inline xs)
  | Text s ->
      Atom s
  | Emph (_, _, x) ->
      List [Atom "emph"; inline x]
  | Code _ ->
      Atom "code"
  | Hard_break ->
      Atom "hard-break"
  | Soft_break ->
      Atom "soft-break"
  | Url def ->
      List [Atom "url"; link_def inline def]
  | Url_ref (label, def) ->
      List [Atom "url-ref"; inline label; link_def atom def]
  | Html s ->
      List [Atom "html"; Atom s]
  | Img _ ->
      Atom "img"
  | Img_ref (label, def) ->
      List [Atom "img-ref"; inline label; link_def atom def]

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
  | Code_block (None, Some s) ->
      List [Atom "indented-code"; Atom s]
  | Code_block (None, None) ->
      List [Atom "indented-code"]
  | Code_block (Some _, Some s) ->
      List [Atom "fenced-code"; Atom s]
  | Code_block (Some _, None) ->
      List [Atom "fenced-code"]
  | Html_block s ->
      List [Atom "html"; Atom s]
  | Link_def {label; destination; _} ->
      List [Atom "link-def"; Atom label; Atom destination]

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

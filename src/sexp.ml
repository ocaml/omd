open Ast

type t =
  | Atom of string
  | List of t list

let atom s = Atom s

let rec link_def : 'a. ('a -> t) -> 'a link_def -> t = fun f {ld_label; ld_destination; ld_title; _} ->
  let title = match ld_title with Some title -> [Atom title] | None -> [] in
  List (Atom "link-def" :: f ld_label :: Atom ld_destination :: title)

and inline = function
  | Concat xs ->
      List (Atom "concat" :: List.map inline xs)
  | Text s ->
      Atom s
  | Emph e ->
      List [Atom "emph"; inline e.em_content]
  | Code _ ->
      Atom "code"
  | Hard_break ->
      Atom "hard-break"
  | Soft_break ->
      Atom "soft-break"
  | Link {lk_kind = Url; lk_def; _} ->
      List [Atom "url"; link_def inline lk_def]
  | Ref {rf_kind = Url; rf_label; rf_def; _} ->
      List [Atom "url-ref"; inline rf_label; link_def atom rf_def]
  | Html s ->
      List [Atom "html"; Atom s]
  | Link {lk_kind = Img; _} ->
      Atom "img"
  | Ref {rf_kind = Img; rf_label; rf_def; _} ->
      List [Atom "img-ref"; inline rf_label; link_def atom rf_def]
  | Tag {tg_name; tg_content; _} ->
      List [Atom "tag"; Atom tg_name; inline tg_content]

let rec block = function
  | Paragraph x ->
      List [Atom "paragraph"; inline x]
  | List l ->
      List (Atom "list" :: List.map (fun xs -> List (Atom "list-item" :: List.map block xs)) l.bl_blocks)
  | Blockquote xs ->
      List (Atom "blockquote" :: List.map block xs)
  | Thematic_break ->
      Atom "thematic-break"
  | Heading {h_level; h_text; _} ->
      List [Atom "heading"; Atom (string_of_int h_level); inline h_text]
  | Code_block {cb_kind = None; cb_code = Some s; _} ->
      List [Atom "indented-code"; Atom s]
  | Code_block {cb_kind = None; cb_code = None; _} ->
      List [Atom "indented-code"]
  | Code_block {cb_kind = Some _; cb_code = Some s; _} ->
      List [Atom "fenced-code"; Atom s]
  | Code_block {cb_kind = Some _; cb_code = None; _} ->
      List [Atom "fenced-code"]
  | Html_block s ->
      List [Atom "html"; Atom s]
  | Link_def {ld_label; ld_destination; _} ->
      List [Atom "link-def"; Atom ld_label; Atom ld_destination]
  | Def_list {dl_content} ->
      List [Atom "def-list"; List (List.map (fun elt -> List [inline elt.de_term; List (List.map inline elt.de_defs)]) dl_content)]
  | Tag_block {tb_tag; tb_content; _} ->
      List [Atom "tag"; Atom tb_tag; List (List.map block tb_content)]

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

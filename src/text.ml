open Ast

let rec inline b = function
  | Concat l ->
      List.iter (inline b) l
  | Text t ->
      Buffer.add_string b t
  | Emph (_, _, md) ->
      inline b md
  | Code s ->
      Buffer.add_string b s
  | Hard_break | Soft_break ->
      Buffer.add_char b '\n'
  | Html body ->
      Buffer.add_string b body
  | Link (_, {label; _}) | Ref (_, label, _) ->
      inline b label

let block f b = function
  | Paragraph x ->
      f b x
  | _ ->
      assert false
  (* | List of list_kind * list_style * 'a block list list *)
  (* | Blockquote of 'a block list *)
  (* | Thematic_break *)
  (* | Heading of int * 'a *)
  (* | Code_block of (fenced_code_kind * string) option * string option *)
  (* | Html_block of string *)
  (* | Link_def of 'a link_def *)

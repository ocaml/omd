open Ast

let rec inline b = function
  | Concat l ->
      List.iter (inline b) l
  | Text t ->
      Buffer.add_string b t
  | Emph e ->
      inline b e.md
  | Code (n, s) ->
      let d = String.make n '`' in
      Printf.bprintf b "%s%s%s" d s d
  | Hard_break | Soft_break ->
      Buffer.add_char b '\n'
  | Html body ->
      Buffer.add_string b body
  | Link l ->
      inline b l.def.label
  | Ref r ->
      inline b r.label

let block f b = function
  | Paragraph x ->
      f b x
  | _ ->
      assert false

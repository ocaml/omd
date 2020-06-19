open Ast

let rec inline b = function
  | Inline.Concat l ->
      List.iter (inline b) l
  | Text t ->
      Buffer.add_string b t
  | Emph e ->
      inline b e.content
  | Code c ->
      let d = String.make c.level '`' in
      Printf.bprintf b "%s%s%s" d c.content d
  | Hard_break | Soft_break ->
      Buffer.add_char b '\n'
  | Html body ->
      Buffer.add_string b body
  | Link l ->
      inline b l.def.label
  | Ref r ->
      inline b r.label
  | Tag t ->
      inline b t.content

let block f b = function
  | Block.Paragraph x ->
      f b x
  | _ ->
      assert false

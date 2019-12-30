open Ast

let rec inline b = function
  | Concat l ->
      List.iter (inline b) l
  | Text t ->
      Buffer.add_string b t
  | Emph {em_content; _} ->
      inline b em_content
  | Code c ->
      let d = String.make c.c_level '`' in
      Printf.bprintf b "%s%s%s" d c.c_content d
  | Hard_break | Soft_break ->
      Buffer.add_char b '\n'
  | Html body ->
      Buffer.add_string b body
  | Link l ->
      inline b l.lk_def.ld_label
  | Ref r ->
      inline b r.rf_label
  | Tag t ->
      inline b t.tg_content

let block f b = function
  | Paragraph x ->
      f b x
  | _ ->
      assert false

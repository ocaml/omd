open Ast

let escape_markdown_characters s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '.' as c ->
        if i > 0 &&
           match s.[i-1] with
           | '0' .. '9' -> i+1 < String.length s && s.[i+1] = ' '
           | _ -> false
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '-' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && (s.[i+1] = ' '||s.[i+1] = '-'))
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '+' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && s.[i+1] = ' ')
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '!' as c ->
        if i+1 < String.length s && s.[i+1] = '[' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '<' as c ->
        if i <> String.length s - 1 &&
           (match s.[i+1] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true)
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '>' as c ->
        if i = 0 || (match s.[i-1] with ' ' | '\n' -> false | _ -> true) then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '#' as c ->
        if i = 0 || s.[i-1] = '\n' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '\\' | '[' | ']' | '(' | ')' | '`' | '*' as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let print_attributes b a =
  match a with
  | {a_id = None; a_classes = []; a_attributes = []} -> ()
  | _ ->
      let start = ref false in
      Printf.bprintf b "{";
      begin match a.a_id with
      | None -> ()
      | Some s ->
          start := true;
          Printf.bprintf b "#%s" s
      end;
      begin match a.a_classes with
      | [] -> ()
      | l ->
          let dump s =
            if !start then
              Printf.bprintf b " ";
            Printf.bprintf b ".%s" s;
            start := true
          in
          List.iter dump l
      end;
      begin match a.a_attributes with
      | [] -> ()
      | l ->
          let dump (n,v) =
            if !start then
              Printf.bprintf b " ";
            Printf.bprintf b "%s=%s" n v;
            start := true
          in
          List.iter dump l
      end;
      Printf.bprintf b "}"

let rec inline b = function
  | Concat l ->
      List.iter (inline b) l
  | Text t ->
      Printf.bprintf b "%s" (escape_markdown_characters t)
  | Emph {em_kind = Normal; em_style; em_content} ->
      let q = match em_style with Star -> '*' | Underscore -> '_' in
      Printf.bprintf b "%c%a%c" q inline em_content q
  | Emph {em_kind = Strong; em_style; em_content} ->
      let q = match em_style with Star -> '*' | Underscore -> '_' in
      Printf.bprintf b "%c%c%a%c%c" q q inline em_content q q
  | Code {c_level; c_content; c_attributes} ->
      let d = String.make c_level '`' in
      Printf.bprintf b "%s%s%s" d c_content d;
      print_attributes b c_attributes
  | Hard_break ->
      Buffer.add_string b "<br />"
  | Html body ->
      Buffer.add_string b body
  | Link {lk_kind = Url; lk_def = {ld_label; ld_destination; ld_title = None; ld_attributes}} ->
      Printf.bprintf b "[%a](%s)" inline ld_label ld_destination;
      print_attributes b ld_attributes
  | Link {lk_kind = Img; lk_def = {ld_label; ld_destination; ld_title = None; ld_attributes}} ->
      Printf.bprintf b "![%a](%s)" inline ld_label (* FIXME *) ld_destination;
      print_attributes b ld_attributes
  | Link {lk_kind = Url; lk_def = {ld_label; ld_destination; ld_title = Some title; ld_attributes}} ->
      Printf.bprintf b "[%a](%s \"%s\")" inline ld_label ld_destination title;
      print_attributes b ld_attributes
  | Link {lk_kind = Img; lk_def = {ld_label; ld_destination; ld_title = Some title; ld_attributes}} ->
      Printf.bprintf b "![%a](%s \"%s\")" inline ld_label (* FIXME *) ld_destination title;
      print_attributes b ld_attributes
  | Ref {rf_kind = Url; rf_label; rf_def = {ld_label; _}} ->
      Printf.bprintf b "[%a][%s]" inline rf_label ld_label
  | Ref {rf_kind = Img; rf_label; rf_def = {ld_label; _}} ->
      Printf.bprintf b "![%a][%s]" inline rf_label ld_label
  | Soft_break ->
      if Buffer.length b = 1 ||
         (Buffer.length b > 1 &&
          not(Buffer.nth b (Buffer.length b - 1) = '\n' &&
              Buffer.nth b (Buffer.length b - 2) = '\n'))
      then
        Buffer.add_string b "\n"
  | Tag {tg_name; tg_content; tg_attributes} ->
      Printf.bprintf b "{!%s:%a}" tg_name inline tg_content;
      print_attributes b tg_attributes

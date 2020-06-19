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
  | {Attributes.id = None; classes = []; attributes = []} -> ()
  | _ ->
      let start = ref false in
      Printf.bprintf b "{";
      begin match a.id with
      | None -> ()
      | Some s ->
          start := true;
          Printf.bprintf b "#%s" s
      end;
      begin match a.classes with
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
      begin match a.attributes with
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
  | Inline.Concat l ->
      List.iter (inline b) l
  | Text t ->
      Printf.bprintf b "%s" (escape_markdown_characters t)
  | Emph {kind = Normal; style; content} ->
      let q = match style with Star -> '*' | Underscore -> '_' in
      Printf.bprintf b "%c%a%c" q inline content q
  | Emph {kind = Strong; style; content} ->
      let q = match style with Star -> '*' | Underscore -> '_' in
      Printf.bprintf b "%c%c%a%c%c" q q inline content q q
  | Code {level; content; attributes} ->
      let d = String.make level '`' in
      Printf.bprintf b "%s%s%s" d content d;
      print_attributes b attributes
  | Hard_break ->
      Buffer.add_string b "<br />"
  | Html body ->
      Buffer.add_string b body
  | Link {kind = Url; def = {label; destination; title = None; attributes}} ->
      Printf.bprintf b "[%a](%s)" inline label destination;
      print_attributes b attributes
  | Link {kind = Img; def = {label; destination; title = None; attributes}} ->
      Printf.bprintf b "![%a](%s)" inline label (* FIXME *) destination;
      print_attributes b attributes
  | Link {kind = Url; def = {label; destination; title = Some title; attributes}} ->
      Printf.bprintf b "[%a](%s \"%s\")" inline label destination title;
      print_attributes b attributes
  | Link {kind = Img; def = {label; destination; title = Some title; attributes}} ->
      Printf.bprintf b "![%a](%s \"%s\")" inline label (* FIXME *) destination title;
      print_attributes b attributes
  | Ref {kind = Url; label; def = {label = label'; _}} ->
      Printf.bprintf b "[%a][%s]" inline label label'
  | Ref {kind = Img; label; def = {label = label'; _}} ->
      Printf.bprintf b "![%a][%s]" inline label label'
  | Soft_break ->
      if Buffer.length b = 1 ||
         (Buffer.length b > 1 &&
          not(Buffer.nth b (Buffer.length b - 1) = '\n' &&
              Buffer.nth b (Buffer.length b - 2) = '\n'))
      then
        Buffer.add_string b "\n"
  | Tag {tag; content; attributes} ->
      Printf.bprintf b "{!%s:%a}" tag inline content;
      print_attributes b attributes

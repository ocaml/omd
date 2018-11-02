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

let rec markdown_of_md md =
  let b = Buffer.create 64 in
  let rec loop = function
    | Concat l ->
        List.iter loop l
    | Text t ->
        Printf.bprintf b "%s" (escape_markdown_characters t)
    | Emph (Normal, q, md) ->
        let delim = String.make 2 (match q with Star -> '*' | Underscore -> '_') in
        Buffer.add_string b delim;
        loop md;
        Buffer.add_string b delim
    | Emph (Strong, q, md) ->
        let delim = String.make 2 (match q with Star -> '*' | Underscore -> '_') in
        Buffer.add_string b delim;
        loop md;
        Buffer.add_string b delim;
    | Code c ->
        let n = (* compute how many backquotes we need to use *)
          let filter (n:int) (s:int list) =
            if n > 0 && n < 10 then
              List.filter (fun e -> e <> n) s
            else
              s
          in
          let l = String.length c in
          let rec loop s x b i =
            if i = l then begin
              match filter b s with hd :: _ -> hd | [] -> x+1
            end else begin
              match c.[i] with
              | '`' ->
                  loop s x (succ b) (succ i)
              | _ ->
                  loop (filter b s) (max b x) 0 (succ i)
            end
          in
          loop [1;2;3;4;5;6;7;8;9;10] 0 0 0
        in
        Printf.bprintf b "%s" (String.make n '`');
        if c.[0] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" c;
        if c.[String.length c - 1] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" (String.make n '`')
    | Hard_break ->
        Buffer.add_string b "<br />"
    | Html body ->
        Buffer.add_string b body
    | Url {label; destination; title = None} ->
        Printf.bprintf b "[%s](%s)" (markdown_of_md label) destination
    | Img {label; destination; title = None} ->
        Printf.bprintf b "![%s](%s)" (markdown_of_md label (* FIXME *)) destination
    | Url {label; destination; title = Some title} ->
        Printf.bprintf b "[%s](%s \"%s\")" (markdown_of_md label) destination title
    | Img {label; destination; title = Some title} ->
        Printf.bprintf b "![%s](%s \"%s\")" (markdown_of_md label (* FIXME *)) destination title
    | Url_ref (label, {Ast.label = label1; _}) ->
        Printf.bprintf b "[%s][%s]" (markdown_of_md label) (markdown_of_md label1)
    | Img_ref (label, {Ast.label = label1; _}) ->
        Printf.bprintf b "![%s][%s]" (markdown_of_md label) (markdown_of_md label1)
    | Soft_break ->
        if Buffer.length b = 1 ||
           (Buffer.length b > 1 &&
            not(Buffer.nth b (Buffer.length b - 1) = '\n' &&
                Buffer.nth b (Buffer.length b - 2) = '\n'))
        then
          Buffer.add_string b "\n"
  in
  loop md;
  Buffer.contents b

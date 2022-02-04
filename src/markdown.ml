open Ast

type t = attributes block list

let nchar n buf c =
  let rec aux n =
    if n <= 0 then
      ()
    else (
      Buffer.add_char buf c;
      aux (n - 1)
    )
  in
  aux n

let nl buf = Buffer.add_char buf '\n'

let sp buf = Buffer.add_char buf ' '

let add_string_escape_chars b s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '.' as c ->
        if
          i > 0
          &&
          match s.[i - 1] with
          | '0' .. '9' -> i + 1 < String.length s && s.[i + 1] = ' '
          | _ -> false
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '-' as c ->
        if
          (i = 0
          ||
          match s.[i - 1] with
          | ' '
          | '\n' ->
              true
          | _ -> false)
          && i + 1 < String.length s
          && (s.[i + 1] = ' ' || s.[i + 1] = '-')
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '+' as c ->
        if
          (i = 0
          ||
          match s.[i - 1] with
          | ' '
          | '\n' ->
              true
          | _ -> false)
          && i + 1 < String.length s
          && s.[i + 1] = ' '
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '!' as c ->
        if i + 1 < String.length s && s.[i + 1] = '[' then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '<' as c ->
        if
          i <> String.length s - 1
          &&
          match s.[i + 1] with
          | 'a' .. 'z'
          | 'A' .. 'Z' ->
              false
          | _ -> true
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '>' as c ->
        if
          i = 0
          ||
          match s.[i - 1] with
          | ' '
          | '\n' ->
              false
          | _ -> true
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '#' as c ->
        if i = 0 || s.[i - 1] = '\n' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | ('\\' | '[' | ']' | '(' | ')' | '`' | '*') as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c -> Buffer.add_char b c
  done

let infer_num_backticks min_allowed c =
  let l = String.length c in
  let rec loop m s i =
    if i = l then
      max m s
    else
      match c.[i] with
      | '`' -> loop m (s + 1) (i + 1)
      | _ -> loop (max m s) 1 (i + 1)
  in
  loop min_allowed 1 0

let add_attrs_to_buffer ?(space = false) buf attrs =
  let add_attr (k, v) =
    match k with
    | "class" ->
        String.split_on_char ' ' v
        |> List.map (Printf.sprintf ".%s")
        |> String.concat " "
        |> Buffer.add_string buf
    | "id" -> Printf.bprintf buf "#%s" v
    | k -> Printf.bprintf buf "%s=%s" k v
  in
  match attrs with
  | [] -> ()
  | attr :: attrs ->
      if space then sp buf;
      Buffer.add_char buf '{';
      add_attr attr;
      List.iter
        (fun (k, v) ->
          sp buf;
          add_attr (k, v))
        attrs;
      Buffer.add_char buf '}'

let rec inline ~prefix buf = function
  | Ast.Concat (_, l) -> List.iter (inline ~prefix buf) l
  | Text (_, t) -> add_string_escape_chars buf t
  | Emph (_, il) ->
      Buffer.add_char buf '*';
      inline ~prefix buf il;
      Buffer.add_char buf '*'
  | Strong (_, il) ->
      Buffer.add_string buf "**";
      inline ~prefix buf il;
      Buffer.add_string buf "**"
  | Code (attr, c) ->
      let n = String.length c in
      let num_backticks = infer_num_backticks 1 c in
      nchar num_backticks buf '`';
      if n > 0 && c.[0] = '`' then sp buf;
      Buffer.add_string buf c;
      if n > 0 && c.[n - 1] = '`' then sp buf;
      nchar num_backticks buf '`';
      add_attrs_to_buffer buf attr
  | Hard_break _attr ->
      sp buf;
      sp buf;
      nl buf;
      Buffer.add_string buf prefix
  | Soft_break _ ->
      nl buf;
      Buffer.add_string buf prefix
  | Html (_, body) -> Buffer.add_string buf body
  | Link (attr, link) -> add_link ~prefix buf attr link
  | Image (attr, link) ->
      Buffer.add_char buf '!';
      add_link ~prefix buf attr link

and add_link ~prefix buf attr { label; destination; title } =
  Buffer.add_char buf '[';
  inline ~prefix buf label;
  Buffer.add_char buf ']';
  Buffer.add_char buf '(';
  Buffer.add_string buf destination;
  Option.iter (Printf.bprintf buf " %S") title;
  Buffer.add_char buf ')';
  add_attrs_to_buffer buf attr

let add_nl_with_quote_prefix ~prefix ~quote ~tight buf =
  if (not tight) && Buffer.length buf > 1 then begin
    if quote then Buffer.add_string buf prefix;
    nl buf
  end

let add_prefix ~first_item_prefix ~prefix buf =
  let prefix = Option.value ~default:prefix first_item_prefix in
  Buffer.add_string buf prefix

let add_string_with_nl_prefix ~first_item_prefix ~prefix buf s =
  String.iter
    (fun x ->
      Buffer.add_char buf x;
      if x = '\n' then add_prefix ~first_item_prefix ~prefix buf)
    s

let rec block ?first_item_prefix ~prefix ~quote ~tight buf b =
  match b with
  | Blockquote (_attr, q) -> begin
      match q with
      | [] -> ()
      | hd :: tl ->
          let first_item_prefix =
            Option.map (fun s -> s ^ "> ") first_item_prefix
          in
          let prefix = prefix ^ "> " in
          block ?first_item_prefix ~prefix ~quote ~tight buf hd;
          List.iter (block ?first_item_prefix ~prefix ~quote:true ~tight buf) tl
    end
  | Paragraph (_attr, md) ->
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      add_prefix ~first_item_prefix ~prefix buf;
      inline ~prefix buf md;
      nl buf
  | List (_attr, ty, spacing, bl) -> (
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      let tight =
        match spacing with
        | Loose -> false
        | Tight -> true
      in
      match ty with
      | Ordered (x, c) ->
          add_ordered_list ?first_item_prefix ~prefix ~quote ~tight buf x c bl
      | Bullet c ->
          add_bullet_list ?first_item_prefix ~prefix ~quote ~tight buf c bl)
  | Code_block (attr, label, c) ->
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      add_prefix ~first_item_prefix ~prefix buf;
      add_code_block ~first_item_prefix ~prefix buf attr label c
  | Thematic_break _attr ->
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      add_prefix ~first_item_prefix ~prefix buf;
      nchar 3 buf '*';
      nl buf
  | Html_block (_, body) ->
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      add_prefix ~first_item_prefix ~prefix buf;
      let n = String.length body in
      let body =
        if n > 0 && body.[n - 1] = '\n' then
          String.sub body 0 (n - 1)
        else
          body
      in
      add_string_with_nl_prefix ~first_item_prefix ~prefix buf body;
      nl buf
  | Heading (attr, level, text) ->
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      add_prefix ~first_item_prefix ~prefix buf;
      if 0 < level && level < 7 then (
        nchar level buf '#';
        sp buf
      );
      inline ~prefix buf text;
      add_attrs_to_buffer ~space:true buf attr;
      nl buf
  | Definition_list (_attr, l) ->
      add_nl_with_quote_prefix ~prefix ~quote ~tight buf;
      add_prefix ~first_item_prefix ~prefix buf;
      add_def_list ~prefix buf l

and add_ordered_list ?first_item_prefix ~prefix ~quote ~tight buf x c = function
  | [] -> ()
  | hd :: tl ->
      let add_list_item before_first_item_prefix i bl =
        let n, s =
          let s = Printf.sprintf "%d%c  " (x + i) c in
          (String.length s, s)
        in
        let first_item_prefix = before_first_item_prefix ^ s in
        let prefix = prefix ^ String.make n ' ' in
        match bl with
        | [] -> ()
        | hd :: tl ->
            block ~first_item_prefix ~prefix ~quote ~tight buf hd;
            List.iter (fun b -> block ~prefix ~quote ~tight buf b) tl
      in
      add_list_item (Option.value ~default:prefix first_item_prefix) 0 hd;
      List.iteri (fun i -> add_list_item prefix (i + 1)) tl

and add_code_block ~first_item_prefix ~prefix buf attr label c =
  let n = infer_num_backticks 3 c in
  nchar n buf '`';
  let label = String.trim label in
  Buffer.add_string buf label;
  add_attrs_to_buffer ~space:true buf attr;
  nl buf;
  add_prefix ~first_item_prefix ~prefix buf;
  add_string_with_nl_prefix ~first_item_prefix ~prefix buf c;
  let n = String.length c in
  if n > 0 && c.[String.length c - 1] <> '\n' then begin
    nl buf;
    add_prefix ~first_item_prefix ~prefix buf
  end;
  nchar n buf '`';
  nl buf

and add_bullet_list ?first_item_prefix ~prefix ~quote ~tight buf c = function
  | [] -> ()
  | hd :: tl ->
      let n, s =
        let s = Printf.sprintf "%c  " c in
        (3, s)
      in
      let add_list_item before_first_item_prefix bl =
        let first_item_prefix = before_first_item_prefix ^ s in
        let prefix = prefix ^ String.make n ' ' in
        match bl with
        | [] -> ()
        | hd :: tl ->
            block ~first_item_prefix ~prefix ~quote ~tight buf hd;
            List.iter (fun b -> block ~prefix ~quote ~tight buf b) tl
      in
      add_list_item (Option.value ~default:prefix first_item_prefix) hd;
      List.iter (add_list_item prefix) tl

and add_def_list ~prefix buf l =
  let add_term { term; defs } =
    inline ~prefix buf term;
    nl buf;
    List.iter
      (fun def ->
        Buffer.add_char buf ':';
        sp buf;
        inline ~prefix buf def;
        nl buf)
      defs
  in
  match l with
  | [] -> ()
  | term :: terms ->
      add_term term;
      List.iter
        (fun term ->
          nl buf;
          add_term term)
        terms

let to_string (t : t) =
  let buf = Buffer.create 1024 in
  List.iter (block ~prefix:"" ~quote:false ~tight:false buf) t;
  Buffer.contents buf

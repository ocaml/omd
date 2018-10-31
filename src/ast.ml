type list_kind =
  | Ordered of int * char
  | Unordered of char

let same_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Unordered c1, Unordered c2 -> c1 = c2
  | _ -> false

type list_style =
  | Loose
  | Tight

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type 'a block =
  | Paragraph of 'a
  | List of list_kind * list_style * 'a block list list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of string * string option
  | Html_block of string
  | Link_def of 'a link_def

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List (k, st, xs) -> List (k, st, List.map (List.map (map f)) xs)
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading (i, x) -> Heading (i, f x)
  | Code_block _ as x -> x
  | Html_block _ as x -> x
  | Link_def {label; destination; title} -> Link_def {label = f label; destination; title}

let extract_defs ast =
  let rec loop acc = function
    | List (_, _, l) -> List.fold_left (List.fold_left loop) acc l
    | Blockquote l -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

type inline =
  | Concat of inline list
  | Text of string
  | Emph of emph_kind * emph_style * inline
  | Code of string
  | Hard_break
  | Soft_break
  | Url of inline * string * string option
  (* | Ref of string * string *)
  (* | Img_ref of string * string *)
  | Html of string
  | Img of string * string * string

let concat = function
  | [x] -> x
  | l -> Concat l

let condense_ws s =
  let b = Buffer.create (String.length s) in
  let rec loop prev_ws start i =
    if i >= String.length s then
      Buffer.contents b
    else begin
      match String.get s i with
      | ' ' | '\t' | '\010'..'\013' ->
          loop true start (succ i)
      | _ as c ->
          if not start && prev_ws then Buffer.add_char b ' ';
          Buffer.add_char b c;
          loop false false (succ i)
    end
  in
  loop false true 0

let starts_with_ws s =
  if String.length s = 0 then
    false
  else begin
    match s.[0] with
    | ' ' | '\t' | '\010'..'\013' ->
        true
    | _ ->
        false
  end

let normalize x =
  let rec f x (ws, cont) =
    match x with
    | Concat l ->
        List.fold_right f l (ws, cont)
    | Text s when String.trim s = "" ->
        (ws, cont)
    | Text s ->
        let ws' = starts_with_ws s in
        let s = String.lowercase_ascii (condense_ws s) in
        let s, cont =
          match ws, cont with
          | true, Text s' :: cont ->
              String.concat " " [s; s'], cont
          | false, Text s' :: cont ->
              s ^ s', cont
          | true, cont ->
              s ^ " ", cont
          | false, cont ->
              s, cont
        in
        ws', Text s :: cont
    | Emph (k, s, x) ->
        let cont = if ws then Text " " :: cont else cont in
        let ws, x = f x (false, []) in
        ws, Emph (k, s, concat x) :: cont
    | Hard_break | Soft_break ->
        (true, cont)
    | _ ->
        assert false
  in
  let _, x = f x (false, []) in
  concat x

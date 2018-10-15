{
type list_item_kind =
  | Ordered of int
  | Bullet of char

let remove_trailing_hashes s =
  let c =
    let c = ref 0 in
    try
      for i = String.length s - 1 downto 0 do
        if s.[i] = '#' then incr c else raise Exit
      done;
      String.length s
    with Exit ->
      !c
  in
  String.sub s 0 (String.length s - c)
}

let ws = [' ''\t']*

rule is_thematic_break = parse
  | ' '? ' '? ' '? '*' ws '*' ws '*' ws eof
  | ' '? ' '? ' '? '_' ws '_' ws '_' ws eof
  | ' '? ' '? ' '? '-' ws '-' ws '-' ws eof { true }
  | _ | eof { false }

and is_empty = parse
  | ws* eof { true }
  | _ { false }

and is_blockquote = parse
  | ' '? ' '? ' '? '>' ' '? { Some (String.length (Lexing.lexeme lexbuf)) }
  | _ | eof { None }

and is_list_item = parse
  | (['+''-''*'] as marker) (' ' ' '? ' '? ' '? as sp)
      { Some (Bullet marker, String.length sp + 1) }
  | (['0'-'9']+ as num) ('.' | ')') (' ' ' '? ' '? ' '? as sp)
      { Some (Ordered (int_of_string num), String.length sp + String.length num + 1) }
  | _ | eof
      { None }

and indent acc = parse
  | ' ' { indent (acc + 1) lexbuf }
  | '\t' { indent (acc + 4) lexbuf }
  | _ | eof { acc }

and is_atx_heading = parse
  | ' '? ' '? ' '? ("#" | "##" | "###" | "####" | "#####" | "######" as atx)
      (ws+ (_+ as title) | ("" as title)) eof
    { Some (String.length atx, String.trim (remove_trailing_hashes (String.trim title))) }
  | _ | eof
    { None }

and is_fenced_code = parse
  | (' '? ' '? ' '? as ind) ("~~~" '~'* | "```" '`'* as delim) (_* as info)
      { Some (String.length ind, String.length delim, String.trim info) }
  | _ | eof
    { None }

{
let is_thematic_break s =
  is_thematic_break (Lexing.from_string s)

let is_empty s =
  is_empty (Lexing.from_string s)

let is_blockquote s =
  is_blockquote (Lexing.from_string s)

let is_list_item s =
  is_list_item (Lexing.from_string s)

let indent s =
  indent 0 (Lexing.from_string s)

let is_atx_heading s =
  is_atx_heading (Lexing.from_string s)

let is_fenced_code s =
  is_fenced_code (Lexing.from_string s)

let is_fenced_code_closing num s =
  match is_fenced_code s with
  | Some (_, num', "") ->
      num' >= num
  | _ ->
      false
}

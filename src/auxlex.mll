{
type list_item_kind =
  | Ordered of int
  | Bullet of char
}

let ws = [' ''\t']*

rule is_thematic_break = parse
  | ' '?' '?' '? '*' ws '*' ws '*' ws eof
  | ' '?' '?' '? '_' ws '_' ws '_' ws eof
  | ' '?' '?' '? '-' ws '-' ws '-' ws eof { true }
  | _ | eof { false }

and is_empty = parse
  | ws* eof { true }
  | _ { false }

and is_blockquote = parse
  | ' '?' '?' '? '>' ' '? { Some (String.length (Lexing.lexeme lexbuf)) }
  | _ | eof { None }

and is_list_item = parse
  | (['+''-''*'] as marker) (' ' ' '? ' '? ' '? as sp)
      { Some (Bullet marker, String.length sp + 1) }
  | (['0'-'9']+ as num) ('.' | ')') (' ' ' '? ' '? ' '? as sp)
      { Some (Ordered (int_of_string num), String.length sp + String.length num + 1) }
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
}

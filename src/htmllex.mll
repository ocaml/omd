{
type html =
  | Closing_tag
  | Open_tag
  | Comment
  | Processing_instruction
  | Declaration
  | CDATA_section

type t =
  | Html of html * string
  | Text of string
  | Email of string
  | Url of string
  | Code_span of string
  | Bang_left_bracket
  | Left_bracket
  (* | Right_bracket *)
  | Strong of bool * bool * int
  (* | Emph of int *)
  | Hard_break
  | Soft_break
  | Link of t list * string * string option

let add_char buf c =
  Buffer.add_char buf c

let add_string buf s =
  Buffer.add_string buf s

let int_of_entity _ =
  failwith "int_of_entity"

let decode_entity s =
  match s.[1], s.[2] with
  | '#', ('x' | 'X') ->
      int_of_string ("0x" ^ String.sub s 3 (String.length s - 4))
  | '#', _ ->
      int_of_string (String.sub s 2 (String.length s - 3))
  | _ ->
      int_of_entity (String.sub s 1 (String.length s - 2))

let add_entity buf e =
  Buffer.add_utf_8_uchar buf (Uchar.of_int (decode_entity e))

let text buf acc =
  if Buffer.length buf = 0 then
    acc
  else begin
    let s = Buffer.contents buf in
    Buffer.clear buf;
    Text s :: acc
  end

let lexeme_length lexbuf =
  Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf

let add_lexeme buf lexbuf =
  Buffer.add_string buf (Lexing.lexeme lexbuf)

let copy_lexbuf
    { Lexing.refill_buff; lex_buffer; lex_buffer_len; lex_abs_pos;
      lex_start_pos; lex_curr_pos; lex_last_pos; lex_last_action;
      lex_eof_reached; lex_mem; lex_start_p; lex_curr_p }
  =
  { Lexing.refill_buff; lex_buffer; lex_buffer_len; lex_abs_pos;
    lex_start_pos; lex_curr_pos; lex_last_pos; lex_last_action;
    lex_eof_reached; lex_mem; lex_start_p; lex_curr_p }

let raw_html inline f acc buf0 lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  let buf = Buffer.create 17 in
  add_lexeme buf lexbuf;
  match f buf lexbuf with
  | kind ->
      inline (Html (kind, Buffer.contents buf) :: text buf0 acc) buf0 lexbuf
  | exception _ ->
      add_lexeme buf0 lexbuf0;
      inline acc buf0 lexbuf0

let code inline f acc buf0 lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  let buf = Buffer.create 17 in
  match f buf lexbuf with
  | () ->
      inline (Code_span (Buffer.contents buf) :: text buf0 acc) buf0 lexbuf
  | exception _ ->
      add_lexeme buf0 lexbuf0;
      inline acc buf0 lexbuf0

let is_ws = function
  | ' ' | '\t' | '\010'..'\013' -> true
  | _ -> false

let is_punct = function
  | '!' | '"' | '#' | '$' | '%'
  | '&' | '\'' | '(' | ')' | '*' | '+'
  | ',' | '-' | '.' | '/' | ':' | ';'
  | '<' | '=' | '>' | '?' | '@' | '['
  | '\\' | ']' | '^' | '_' | '`' | '{'
  | '|' | '}' | '~' -> true
  | _ -> false

let protect f lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  match f lexbuf with
  | x ->
      Ok x
  | exception _ ->
      Error lexbuf0
}

let ws = [' ''\t''\010'-'\013']
(* let non_ws = [^' ''\t''\010'-'\013'] *)
(* let punct = ['!''"''#''$''%''&''\'''('')''*''+'',''-''.''/'':'';''<''=''>''?''@''[''\''']''^''_''`''{''|''}''~'] *)
let nl = '\n' | "\r\n" | '\r'
let unquoted_attribute_value = [^' ''\t''\010'-'\013''"''\'''=''<''>''`']+
let single_quoted_attribute_value = '\'' [^'\'']* '\''
let double_quoted_attribute_value = '"' [^'"']* '"'
let attribute_value = unquoted_attribute_value | single_quoted_attribute_value | double_quoted_attribute_value
let attribute_value_specification = ws* '=' ws* attribute_value
let attribute_name = ['a'-'z''A'-'Z''_'':']['a'-'z''A'-'Z''0'-'9''_''.'':''-']*
let attribute = ws+ attribute_name attribute_value_specification?
let tag_name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-']*
let open_tag = '<' tag_name attribute* ws* '/'? '>'
let closing_tag = "</" tag_name ws* '>'
let email = ['a'-'z''A'-'Z''0'-'9''.''!''#''$''%''&''\'''*''+''/''=''?''^''_''`''{''|''}''~''-']+ '@'
            ['a'-'z''A'-'Z''0'-'9'](['a'-'z''A'-'Z''0'-'9''-']['a'-'z''A'-'Z''0'-'9'])?
            ('.'['a'-'z''A'-'Z''0'-'9'](['a'-'z''A'-'Z''0'-'9''-']['a'-'z''A'-'Z''0'-'9'])?)*
let scheme = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''+''-''.']+
let uri = scheme ':' [^' ''\t''\010'-'\013''<''>''\x00'-'\x1F''\x7F''\x80'-'\x9F']*
let sym_entity = '&' ['a'-'z''A'-'Z']+ ';'
let dec_entity = "&#" ['0'-'9']+ ';'
let hex_entity = "&#" ['x''X'] ['0'-'9''a'-'f''A'-'F']+ ';'
let entity = sym_entity | dec_entity | hex_entity

let dest = [^' ''\t''\010'-'\013']+
let title = '"' [^'"']* '"'

rule inline acc buf = parse
  | closing_tag as s          { inline (Html (Closing_tag, s) :: text buf acc) buf lexbuf }
  | open_tag as s             { inline (Html (Open_tag, s) :: text buf acc) buf lexbuf }
  | "<!--"                    { raw_html inline (html_comment true) acc buf lexbuf }
  | "<?"                      { raw_html inline processing_instruction acc buf lexbuf }
  | "<!" ['A'-'Z']+           { raw_html inline declaration acc buf lexbuf }
  | "<![CDATA["               { raw_html inline cdata_section acc buf lexbuf }
  | '<' (email as x) '>'      { inline (Email x :: text buf acc) buf lexbuf }
  | '<' (uri as x) '>'        { inline (Url x :: text buf acc) buf lexbuf }
  | (' ' ' '+ | '\\') nl ws*  { inline (Hard_break :: text buf acc) buf lexbuf }
  | nl                        { inline (Soft_break :: text buf acc) buf lexbuf }
  | '\\' (_ as c)             { add_char buf c; inline acc buf lexbuf }
  | entity as e               { add_entity buf e; inline acc buf lexbuf }
  | '`'+                      { code inline (code_span true false (lexeme_length lexbuf)) acc buf lexbuf }
  | ('*'+|'_'+ as r)
      { let pre = if Lexing.lexeme_start lexbuf > 0 then Bytes.get lexbuf.lex_buffer (Lexing.lexeme_start lexbuf - 1) else ' ' in
        let post = if Lexing.lexeme_end lexbuf < Bytes.length lexbuf.lex_buffer then
          Bytes.get lexbuf.lex_buffer (Lexing.lexeme_end lexbuf) else ' ' in
        let is_left = not (is_ws post) && (not (is_punct post) || is_ws pre || is_punct pre) in
        let is_right = not (is_ws pre) && (not (is_punct pre) || is_ws post || is_punct post) in
        let acc = Strong (is_left, is_right, String.length r) :: text buf acc in
        inline acc buf lexbuf }
  | "!["                      { inline (Bang_left_bracket :: text buf acc) buf lexbuf }
  | '['                       { inline (Left_bracket :: text buf acc) buf lexbuf }
  | ']''('                       {
      let rec loop xs = function
        | Left_bracket :: acc' ->
           begin match protect link_destination lexbuf with
           | Ok (uri, title) -> inline (Link (xs, uri, title) :: acc') buf lexbuf
           | Error lexbuf ->
               add_lexeme buf lexbuf; inline acc buf lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme buf lexbuf; inline acc buf lexbuf
      in
      loop [] (text buf acc)
     }
  | _ as c                    { add_char buf c; inline acc buf lexbuf }
  | eof                       { List.rev (text buf acc) }

and link_destination = parse
  | ws* (dest as d) (ws+ (title as t))? ws* ')' { d, t }

and code_span start seen_ws n buf = parse
  | '`'+          { if lexeme_length lexbuf <> n then begin
                      if not start && seen_ws then add_char buf ' ';
                      add_lexeme buf lexbuf;
                      code_span false false n buf lexbuf
                    end }
  | ws+           { code_span start true n buf lexbuf }
  | _ as c        { if not start && seen_ws then Buffer.add_char buf ' ';
                    add_char buf c;
                    code_span false false n buf lexbuf }

and html_comment start buf = parse
  | "--->"      { raise Exit }
  | "-->"       { add_lexeme buf lexbuf; Comment }
  | "--"        { raise Exit }
  | ">" | "->"  { if start then raise Exit;
                  add_lexeme buf lexbuf;
                  html_comment false buf lexbuf }
  | entity as e { add_entity buf e;
                  html_comment false buf lexbuf }
  | _ as c      { add_char buf c;
                  html_comment false buf lexbuf }

and processing_instruction buf = parse
  | "?>"        { add_lexeme buf lexbuf; Processing_instruction }
  | entity as e { add_entity buf e;
                  processing_instruction buf lexbuf }
  | _ as c      { add_char buf c;
                  processing_instruction buf lexbuf }

and declaration buf = parse
  | '>' as c    { add_char buf c; Declaration }
  | entity as e { add_entity buf e;
                  declaration buf lexbuf }
  | _ as c      { add_char buf c;
                  declaration buf lexbuf }

and cdata_section buf = parse
  | "]]>"       { add_lexeme buf lexbuf; CDATA_section }
  | entity as e { add_entity buf e;
                  cdata_section buf lexbuf }
  | _ as c      { add_char buf c;
                  cdata_section buf lexbuf }

{
let parse s =
  let lexbuf = Lexing.from_string s in
  inline [] (Buffer.create 17) lexbuf
}

{
let buf = Buffer.create 37

let add_char c =
  Buffer.add_char buf c

let add_string s =
  Buffer.add_string b s

let decode_entity s =
  match s.[1], s.[2] with
  | '#', ('x' | 'X') ->
      int_of_string ("0x" ^ String.sub s 3 (String.length s - 4))
  | '#', _ ->
      int_of_string (String.sub s 2 (String.length s - 3))
  | _ ->
      int_of_entity (String.sub s 1 (String.length s - 2))

let add_entity e =
  Buffer.add_utf_8 buf (decode_entity e)

let text acc =
  if Buffer.length buf = 0 then
    acc
  else begin
    let s = Buffer.contents buf in
    Buffer.clear buf;
    Text s :: acc
  end

let lexeme_length lexbuf =
  Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf
}

let ws = [' ''\t''\010'-'\013']
let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let let_dig = letter | digit
let unquoted_attribute_value = [^' ''\t''\010'-'\013''"''\'''=''<''>''`']+
let single_quoted_attribute_value = '\'' [^'\'']* '\''
let double_quoted_attribute_value = '"' [^'"']* '"'
let attribute_value = unquoted_attribute_value | single_quoted_attribute_value | double_quoted_attribute_value
let attribute_value_specification = ws* '=' ws* attribute_value
let attribute_name = ['a'-'z''A'-'Z''_'':']['a'-'z''A'-'Z''0'-'9''_''.'':''-']*
let attribute = ws+ attribute_name attribute_value_specification?
let tag_name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-']*
let open_tag = '<' tag_name attribute+ ws* '/'? '>'
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

rule inline acc = parse
  | closing_tag         { inline (Html (Lexing.lexeme lexbuf) :: text acc) lexbuf }
  | open_tag            { open_tag lexbuf }
  | "<!--"              { let buf = Buffer.create 17 in
                          Buffer.add_string buf (Lexing.lexeme lexbuf);
                          html_comment (text acc) true buf lexbuf }
  | "<?"                { let buf = Buffer.create 17 in
                          Buffer.add_string buf (Lexing.lexeme lexbuf);
                          processing_instruction (text acc) buf lexbuf }
  | "<!" ['A'-'Z']+     { let buf = Buffer.create 17 in
                          Buffer.add_string buf (Lexing.lexeme lexbuf);
                          declaration (text buf) lexbuf }
  | "<![CDATA["         { let buf = Buffer.create 17 in
                          Buffer.add_string buf (Lexing.lexeme lexbuf);
                          cdata_section (text buf) lexbuf }
  | '<' email '>'       { let buf = Buffer.create 17 in (* FIXME *)
                          Buffer.add_buffer buf (Lexing.lexeme lexbuf);
                          email (text acc) buf lexbuf }
  | '<' uri '>'         { }
  | (' ' ' '+ | '\\') '\n' ws*       { } (* HARDBREAK *)
  | '\n'                { } (* SOFTBREAK *)
  | '\\' _ as c         { add_char c; inline acc lexbuf }
  | entity as e         { add_entity e; inline acc lexbuf }
  | '`'+                { code_span (text acc) true false (lexeme_length lexbuf) (Buffer.create 17) lexbuf }
  | '*'+                { }
  | '_'+                { }
  | _ as c              { add_char c; inline acc lexbuf }
  | eof                 { }

and code_span acc start seen_ws n buf = parse
  | '`'+          { if lexeme_length lexbuf = n then
                      inline (Code_span (Buffer.contents buf) :: acc) lexbuf
                    else begin
                      if not start && seen_ws then Buffer.add_char buf ' ';
                      Buffer.add_string buf (Lexing.lexeme lexbuf);
                      code_span acc false false n buf lexbuf
                    end }
  | ws+           { code_span acc start true n buf lexbuf }
  | _ as c        { if not start && seen_ws then Buffer.add_char buf ' ';
                    Buffer.add_char buf c;
                    code_span false acc false false n buf lexbuf }

(* and open_tag = parse *)
(*   | ... *)
(*   | entity { } *)

and html_comment acc start buf = parse
  | "--->"      { raise Exit }
  | "-->"       { Buffer.add_string buf (Lexing.lexeme lexbuf);
                  inline (Html_comment (Buffer.contents buf) :: acc) lexbuf }
  | "--"        { raise Exit }
  | ">" | "->"  { if start then raise Exit;
                  Buffer.add_string buf (Lexing.lexeme lexbuf);
                  html_comment acc false lexbuf }
  | entity as e { add_entity buf e;
                  html_comment acc false buf lexbuf }
  | _ as c      { Buffer.add_char buf c;
                  html_comment acc false buf lexbuf }

and processing_instruction acc buf = parse
  | "?>"        { Buffer.add_string buf (Lexing.lexeme lexbuf);
                  inline (Html (Buffer.contents buf) :: acc) lexbuf }
  | entity as e { add_entity buf e;
                  processing_instruction acc buf lexbuf }
  | _ as c      { Buffer.add_char buf c;
                  processing_instruction buf lexbuf }

and declaration acc buf = parse
  | '>'         { Buffer.add_char buf c;
                  inline (Html (Buffer.contents buf) :: acc) lexbuf }
  | entity as e { add_entity buf e;
                  declaration acc buf lexbuf }
  | _ as c      { Buffer.add_char buf c;
                  declaration acc buf lexbuf }

and cdata_section buf = parse
  | "]]>"       { Buffer.add_string buf (Lexing.lexeme lexbuf);
                  inline (Html (Buffer.contents buf) :: acc) lexbuf }
  | entity as e { add_entity buf e;
                  cdata_section buf lexbuf }
  | _ as c      { Buffer.add_char buf c;
                  cdata_section buf lexbuf }

(* and email acc buf = parse *)
(*   | '@' ['a'-'z''A'-'Z''0'-'9'](['a'-'z''A'-'Z''0'-'9''-']['a'-'z''A'-'Z''0'-'9'])? *)
(*         ('.'['a'-'z''A'-'Z''0'-'9'](['a'-'z''A'-'Z''0'-'9''-']['a'-'z''A'-'Z''0'-'9'])?)* as s '>' *)
(*                 { Buffer.add_string buf s; *)
(*                   inline (Url (Buffer.contents buf) :: acc) lexbuf } *)
(*   | entity as e { add_entity buf e; *)
(*                   email acc buf lexbuf } *)
(*   | ['a'-'z''A'-'Z''0'-'9''.''!''#''$''%''&''\'''*''+''/''=''?''^''_''`''{''|''}''~''-'] as c *)
(*                 { Buffer.add_char buf c; *)
(*                   email acc buf lexbuf } *)

(* and uri = parse *)
(*   | ... *)
(*   | entity      { add_entity } *)

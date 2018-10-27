{
type html =
  | Closing_tag
  | Open_tag
  | Comment
  | Processing_instruction
  | Declaration
  | CDATA_section

type delim =
  | Ws
  | Punct
  | Other

type emph =
  | Star
  | Underscore

type t =
  | Html of html * string
  | Text of string
  | Email of string
  | Url of string
  | Code_span of string
  | Bang_left_bracket
  | Left_bracket
  | Emph of delim * delim * emph * int
  | Hard_break
  | Soft_break
  | Link of t list * string * string option
  | REmph of bool * t list

let left_flanking = function
  | Emph (_, Other, _, _) | Emph ((Ws | Punct), Punct, _, _) -> true
  | _ -> false

let right_flanking = function
  | Emph (Other, _, _, _) | Emph (Punct, (Ws | Punct), _, _) -> true
  | _ -> false

let is_opener = function
  | Emph (pre, _, Underscore, _) as x ->
      left_flanking x && (not (right_flanking x) || pre = Punct)
  | Emph (_, _, Star, _) as x ->
      left_flanking x
  | _ ->
      false

let is_closer = function
  | Emph (_, post, Underscore, _) as x ->
      right_flanking x && (not (left_flanking x) || post = Punct)
  | Emph (_, _, Star, _) as x ->
      right_flanking x
  | _ ->
      false

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

let classify_delim = function
  | '!' | '"' | '#' | '$' | '%'
  | '&' | '\'' | '(' | ')' | '*' | '+'
  | ',' | '-' | '.' | '/' | ':' | ';'
  | '<' | '=' | '>' | '?' | '@' | '['
  | '\\' | ']' | '^' | '_' | '`' | '{'
  | '|' | '}' | '~' -> Punct
  | ' ' | '\t' | '\010'..'\013' -> Ws
  | _ -> Other

let protect f lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  match f lexbuf with
  | x ->
      Ok x
  | exception _ ->
      Error lexbuf0

let rec parse_emph = function
  | Emph (pre, _, q1, n1) as x :: xs as all when is_opener x ->
      let rec loop acc = function
        | Emph (_, post, q2, n2) as x :: xs when is_closer x && q1 = q2 ->
            let is_strong = n1 >= 2 && n2 >= 2 in
            let xs = if n2 > 2 then Emph (Punct, post, q2, n2-2) :: xs else xs in
            let r = REmph (is_strong, parse_emph (List.rev acc)) :: xs in
            let r = if n1 > 2 then Emph (pre, Punct, q1, n1-2) :: r else r in
            parse_emph r
        | x :: xs ->
            loop (x :: acc) xs
        | [] ->
            all
      in
      loop [] xs
  | x :: xs ->
      x :: parse_emph xs
  | [] ->
      []
}

let ws = [' ''\t''\010'-'\013']
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
        let e = if r.[0] = '*' then Star else Underscore in
        let acc = Emph (classify_delim pre, classify_delim post, e, String.length r) :: text buf acc in
        inline acc buf lexbuf }
  | "!["                      { inline (Bang_left_bracket :: text buf acc) buf lexbuf }
  | '['                       { inline (Left_bracket :: text buf acc) buf lexbuf }
  | ']''('                       {
      let rec loop xs = function
        | Left_bracket :: acc' ->
           begin match protect link_destination lexbuf with
           | Ok (uri, title) -> inline (Link (parse_emph xs, uri, title) :: acc') buf lexbuf
           | Error lexbuf ->
               add_lexeme buf lexbuf; inline acc buf lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme buf lexbuf; inline acc buf lexbuf
      in
      loop [] (text buf acc)
     }
  | _ as c                    { add_char buf c; inline acc buf lexbuf }
  | eof                       { parse_emph (List.rev (text buf acc)) }

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

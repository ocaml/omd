{
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
    Inline.R (Text s) :: acc
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
  | () ->
      inline (Inline.R (Html (Buffer.contents buf)) :: text buf0 acc) buf0 lexbuf
  | exception _ ->
      add_lexeme buf0 lexbuf0;
      inline acc buf0 lexbuf0

let code inline f acc buf0 lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  let buf = Buffer.create 17 in
  match f buf lexbuf with
  | () ->
      inline (Inline.R (Code (Buffer.contents buf)) :: text buf0 acc) buf0 lexbuf
  | exception _ ->
      add_lexeme buf0 lexbuf0;
      inline acc buf0 lexbuf0

let protect f lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  match f lexbuf with
  | x ->
      Ok x
  | exception _ ->
      Error lexbuf0

let backtrack lexbuf n =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n
}

let ws = [' ''\t''\010'-'\013']
let sp3 = (' ' (' ' ' '?)?)?
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

rule inline defs acc buf = parse
  | closing_tag as s          { inline defs (Inline.R (Html s) :: text buf acc) buf lexbuf }
  | open_tag as s             { inline defs (Inline.R (Html s) :: text buf acc) buf lexbuf }
  | "<!--"                    { raw_html (inline defs) (html_comment true) acc buf lexbuf }
  | "<?"                      { raw_html (inline defs) processing_instruction acc buf lexbuf }
  | "<!" ['A'-'Z']+           { raw_html (inline defs) declaration acc buf lexbuf }
  | "<![CDATA["               { raw_html (inline defs) cdata_section acc buf lexbuf }
  | '<' (email as x) '>'      { inline defs (Inline.R (Url (Text x, "mailto:" ^ x, None)) :: text buf acc) buf lexbuf }
  | '<' (uri as x) '>'        { inline defs (Inline.R (Url (Text x, x, None)) :: text buf acc) buf lexbuf }
  | (' ' ' '+ | '\\') nl ws*  { inline defs (Inline.R Hard_break :: text buf acc) buf lexbuf }
  | nl                        { inline defs (Inline.R Soft_break :: text buf acc) buf lexbuf }
  | '\\' (_ as c)             { add_char buf c; inline defs acc buf lexbuf }
  | entity as e               { add_entity buf e; inline defs acc buf lexbuf }
  | '`'+                      { code (inline defs) (code_span true false (lexeme_length lexbuf)) acc buf lexbuf }
  | ('*'+|'_'+ as r)
      { let pre = if Lexing.lexeme_start lexbuf > 0 then Bytes.get lexbuf.lex_buffer (Lexing.lexeme_start lexbuf - 1) else ' ' in
        let post = if Lexing.lexeme_end lexbuf < Bytes.length lexbuf.lex_buffer then
          Bytes.get lexbuf.lex_buffer (Lexing.lexeme_end lexbuf) else ' ' in
        let e = if r.[0] = '*' then Inline.Star else Inline.Underscore in
        let acc = Inline.Emph (Inline.classify_delim pre, Inline.classify_delim post, e, String.length r) :: text buf acc in
        inline defs acc buf lexbuf }
  | "!["                      { inline defs (Bang_left_bracket :: text buf acc) buf lexbuf }
  | '['                       { inline defs (Left_bracket :: text buf acc) buf lexbuf }
  | ']''('                 {
      let rec loop xs = function
        | Inline.Left_bracket :: acc' ->
           let f lexbuf = let r = link_dest lexbuf in link_end lexbuf; r in
           begin match protect f lexbuf with
           | Ok (uri, title) ->
               inline defs (Inline.R (Url (Ast.cat (Inline.parse_emph xs), uri, title)) :: acc') buf lexbuf
           | Error lexbuf ->
               add_lexeme buf lexbuf; inline defs acc buf lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme buf lexbuf; inline defs acc buf lexbuf
      in
      loop [] (text buf acc)
     }
  | ']' "[]"?
     {
      let rec loop xs = function
        | Inline.Left_bracket :: acc' ->
           let label = Ast.cat (Inline.parse_emph xs) in
           let s = Ast.normalize_label label in
           begin match List.find_opt (fun {Ast.label; _} -> label = s) defs with
           | Some {destination; title; _} ->
               inline defs (Inline.R (Url (label, destination, title)) :: acc') buf lexbuf
           | None ->
               add_lexeme buf lexbuf; inline defs acc buf lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme buf lexbuf; inline defs acc buf lexbuf
      in
      loop [] (text buf acc)
     }
  | _ as c                    { add_char buf c; inline defs acc buf lexbuf }
  | eof                       { Inline.parse_emph (List.rev (text buf acc)) }

and link_dest = parse
  | ws* '<' { link_dest1 (Buffer.create 17) lexbuf }
  | ws* { link_dest2 0 (Buffer.create 17) lexbuf }

and link_dest1 buf = parse
  | '>' ws+ (['\'''"''('] as d)  { Buffer.contents buf, link_title d (Buffer.create 17) lexbuf }
  | '>' ws* { Buffer.contents buf, None }
  | nl | '<' { failwith "link_dest1 ws" }
  | '\\' (_ as c) { add_char buf c; link_dest1 buf lexbuf }
  | _ as c { add_char buf c; link_dest1 buf lexbuf }

and link_dest2 n buf = parse
  | ws+ (['\'''"''('] as d)  { Buffer.contents buf, link_title d (Buffer.create 17) lexbuf }
  | ['\x00'-'\x1F''\x7F'] { Buffer.contents buf, None }
  | '\\' (_ as c) { add_char buf c; link_dest2 n buf lexbuf }
  | '(' as c { add_char buf c; link_dest2 (succ n) buf lexbuf }
  | ')' as c { if n > 0 then (add_char buf c; link_dest2 (pred n) buf lexbuf) else (backtrack lexbuf 1; Buffer.contents buf, None) }
  | _ as c { add_char buf c; link_dest2 n buf lexbuf }

and link_title d buf = parse
  | '\\' (_ as c)   { add_char buf c; link_title d buf lexbuf }
  | ['\'''"'')'] as c { if c = d then Some (Buffer.contents buf)
                      else (add_char buf c; link_title d buf lexbuf) }
  | _ as c { add_char buf c; link_title d buf lexbuf }

and link_end = parse
  | ws* ')' { () }

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
  | "-->"       { add_lexeme buf lexbuf }
  | "--"        { raise Exit }
  | ">" | "->"  { if start then raise Exit;
                  add_lexeme buf lexbuf;
                  html_comment false buf lexbuf }
  | entity as e { add_entity buf e;
                  html_comment false buf lexbuf }
  | _ as c      { add_char buf c;
                  html_comment false buf lexbuf }

and processing_instruction buf = parse
  | "?>"        { add_lexeme buf lexbuf }
  | entity as e { add_entity buf e;
                  processing_instruction buf lexbuf }
  | _ as c      { add_char buf c;
                  processing_instruction buf lexbuf }

and declaration buf = parse
  | '>' as c    { add_char buf c }
  | entity as e { add_entity buf e;
                  declaration buf lexbuf }
  | _ as c      { add_char buf c;
                  declaration buf lexbuf }

and cdata_section buf = parse
  | "]]>"       { add_lexeme buf lexbuf }
  | entity as e { add_entity buf e;
                  cdata_section buf lexbuf }
  | _ as c      { add_char buf c;
                  cdata_section buf lexbuf }

and link_def acc = parse
  | sp3 '['
      { let f lexbuf =
          let text = link_label (Buffer.create 17) lexbuf in
          let d, t = link_dest lexbuf in
          text, d, t
        in
        match protect f lexbuf with
        | Ok x ->
            link_def (x :: acc) lexbuf
        | Error lexbuf ->
            List.rev acc, lexbuf.Lexing.lex_curr_pos - lexeme_length lexbuf }
  | _ | eof
    { List.rev acc, lexbuf.Lexing.lex_curr_pos - lexeme_length lexbuf }

and link_label buf = parse
  | '\\' (_ as c) { Buffer.add_char buf c; link_label buf lexbuf }
  | ']' ':' { Buffer.contents buf }
  | _ as c { Buffer.add_char buf c; link_label buf lexbuf }

{
let parse defs s =
  let lexbuf = Lexing.from_string s in
  Ast.cat (inline defs [] (Buffer.create 17) lexbuf)
}

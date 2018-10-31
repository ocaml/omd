{
module Pre = Inline.Pre

let strbuf =
  Buffer.create 101

let buf () =
  let s = Buffer.contents strbuf in
  Buffer.clear strbuf;
  s

let add_char c =
  Buffer.add_char strbuf c

let add_string s =
  Buffer.add_string strbuf s

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

let add_entity e =
  Buffer.add_utf_8_uchar strbuf (Uchar.of_int (decode_entity e))

let text acc =
  if Buffer.length strbuf = 0 then
    acc
  else
    Pre.R (Text (buf ())) :: acc

let lexeme_length lexbuf =
  Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf

let add_lexeme lexbuf =
  add_string (Lexing.lexeme lexbuf)

let copy_lexbuf
    { Lexing.refill_buff; lex_buffer; lex_buffer_len; lex_abs_pos;
      lex_start_pos; lex_curr_pos; lex_last_pos; lex_last_action;
      lex_eof_reached; lex_mem; lex_start_p; lex_curr_p }
  =
  { Lexing.refill_buff; lex_buffer; lex_buffer_len; lex_abs_pos;
    lex_start_pos; lex_curr_pos; lex_last_pos; lex_last_action;
    lex_eof_reached; lex_mem; lex_start_p; lex_curr_p }

let raw_html inline f acc lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  add_lexeme lexbuf;
  match f lexbuf with
  | () ->
      inline (Pre.R (Html (buf ())) :: acc) lexbuf
  | exception _ ->
      Buffer.clear strbuf;
      add_lexeme lexbuf0;
      inline acc lexbuf0

let code inline f acc lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  match f lexbuf with
  | () ->
      inline (Pre.R (Code (buf ())) :: acc) lexbuf
  | exception _ ->
      Buffer.clear strbuf;
      add_lexeme lexbuf0;
      inline acc lexbuf0

let protect f lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  Buffer.clear strbuf;
  match f lexbuf with
  | x ->
      Ok x
  | exception _ ->
      Buffer.clear strbuf;
      Error lexbuf0

let backtrack lexbuf n =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n

let peek_before c lexbuf =
  if Lexing.lexeme_start lexbuf > 0 then
    Bytes.get lexbuf.lex_buffer (Lexing.lexeme_start lexbuf - 1)
  else
    c

let peek_after c lexbuf =
  if Lexing.lexeme_end lexbuf < Bytes.length lexbuf.lex_buffer then
    Bytes.get lexbuf.lex_buffer (Lexing.lexeme_end lexbuf)
  else
    c
}

let punct = ['!''"''#''$''%''&''\'''('')''*''+'',''-''.''/'':'';''<''=''>''?''@''[''\\'']''^''_''`''{''|''}''~']
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

rule inline defs acc = parse
  | closing_tag as s          { inline defs (Pre.R (Html s) :: text acc) lexbuf }
  | open_tag as s             { inline defs (Pre.R (Html s) :: text acc) lexbuf }
  | "<!--"                    { let acc = text acc in
                                raw_html (inline defs) (html_comment true) acc lexbuf }
  | "<?"                      { let acc = text acc in
                                raw_html (inline defs) processing_instruction acc lexbuf }
  | "<!" ['A'-'Z']+           { let acc = text acc in
                                raw_html (inline defs) declaration acc lexbuf }
  | "<![CDATA["               { let acc = text acc in
                                raw_html (inline defs) cdata_section acc lexbuf }
  | '<' (email as x) '>'      { inline defs (Pre.R (Url {Ast.label = Text x; destination = "mailto:" ^ x; title = None}) :: text acc) lexbuf }
  | '<' (uri as x) '>'        { inline defs (Pre.R (Url {Ast.label = Text x; destination = x; title = None}) :: text acc) lexbuf }
  | (' ' ' '+ | '\\') nl ws*  { inline defs (Pre.R Hard_break :: text acc) lexbuf }
  | nl                        { inline defs (Pre.R Soft_break :: text acc) lexbuf }
  | '\\' (punct as c)             { add_char c; inline defs acc lexbuf }
  | entity as e               { add_entity e; inline defs acc lexbuf }
  | '`'+                      { let acc = text acc in
                                code (inline defs) (code_span true false (lexeme_length lexbuf)) acc lexbuf }
  | ('*'+|'_'+ as r)
      { let pre = peek_before ' ' lexbuf |> Pre.classify_delim in
        let post = peek_after ' ' lexbuf |> Pre.classify_delim in
        let e = if r.[0] = '*' then Ast.Star else Underscore in
        let acc = Pre.Emph (pre, post, e, String.length r) :: text acc in
        inline defs acc lexbuf }
  | "!["                      { inline defs (Bang_left_bracket :: text acc) lexbuf }
  | '['                       { inline defs (Left_bracket :: text acc) lexbuf }
  | ']''('                 {
      let rec loop xs = function
        | Pre.Left_bracket :: acc' ->
           let f lexbuf = let r = link_dest lexbuf in link_end lexbuf; r in
           begin match protect f lexbuf with
           | Ok (destination, title) ->
               inline defs (Pre.R (Url {Ast.label = Inline.concat (Pre.parse_emph xs); destination; title}) :: acc') lexbuf
           | Error lexbuf ->
               add_lexeme lexbuf; inline defs acc lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme lexbuf; inline defs acc lexbuf
      in
      loop [] (text acc)
     }
  | ']' "[]"?
     {
      let acc = text acc in
      let rec loop xs = function
        | Pre.Left_bracket :: acc' ->
           let label = Inline.concat (Pre.parse_emph xs) in
           let s = Inline.normalize label in
           begin match List.find_opt (fun {Ast.label; _} -> label = s) defs with
           | Some def ->
               inline defs (Pre.R (Url_ref (label, def)) :: acc') lexbuf
           | None ->
               add_lexeme lexbuf; inline defs acc lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme lexbuf; inline defs acc lexbuf
      in
      loop [] acc
     }
  | _ as c                    { add_char c; inline defs acc lexbuf }
  | eof                       { Pre.parse_emph (List.rev (text acc)) }

and link_dest = parse
  | ws* '<' { link_dest1 lexbuf }
  | ws* { link_dest2 0 lexbuf }

and link_dest1 = parse
  | '>' ws+ (['\'''"''('] as d)  { let dest = buf () in
                                   dest, link_title d lexbuf }
  | '>' ws* eof { buf (), None }
  | nl | '<' { failwith "link_dest1 ws" }
  | '\\' (punct as c) { add_char c; link_dest1 lexbuf }
  | _ as c { add_char c; link_dest1 lexbuf }

and link_dest2 n = parse
  | ws* eof { if Buffer.length strbuf > 0 then buf (), None else failwith "link_dest2" }
  | ws+ (['\'''"''('] as d)  { if Buffer.length strbuf > 0 then
                               let dest = buf () in
                               dest, link_title d lexbuf else failwith "link_dest2" }
  | ['\x00'-'\x1F''\x7F'] { buf (), None }
  | '\\' (punct as c) { add_char c; link_dest2 n lexbuf }
  | '(' as c { add_char c; link_dest2 (succ n) lexbuf }
  | ')' as c { if n > 0 then (add_char c; link_dest2 (pred n) lexbuf) else (backtrack lexbuf 1; buf (), None) }
  | _ as c { add_char c; link_dest2 n lexbuf }

and link_title d = parse
  | '\\' (punct as c)   { add_char c; link_title d lexbuf }
  | ['\'''"'')'] as c { if c = d then Some (buf ())
                      else (add_char c; link_title d lexbuf) }
  | _ as c { add_char c; link_title d lexbuf }

and link_end = parse
  | ws* ')' { () }

and code_span start seen_ws n = parse
  | '`'+          { if lexeme_length lexbuf <> n then begin
                      if not start && seen_ws then add_char ' ';
                      add_lexeme lexbuf;
                      code_span false false n lexbuf
                    end }
  | ws+           { code_span start true n lexbuf }
  | _ as c        { if not start && seen_ws then add_char ' ';
                    add_char c;
                    code_span false false n lexbuf }

and html_comment start = parse
  | "--->"      { raise Exit }
  | "-->"       { add_lexeme lexbuf }
  | "--"        { raise Exit }
  | ">" | "->"  { if start then raise Exit;
                  add_lexeme lexbuf;
                  html_comment false lexbuf }
  | entity as e { add_entity e;
                  html_comment false lexbuf }
  | _ as c      { add_char c;
                  html_comment false lexbuf }

and processing_instruction = parse
  | "?>"        { add_lexeme lexbuf }
  | entity as e { add_entity e;
                  processing_instruction lexbuf }
  | _ as c      { add_char c;
                  processing_instruction lexbuf }

and declaration = parse
  | '>' as c    { add_char c }
  | entity as e { add_entity e;
                  declaration lexbuf }
  | _ as c      { add_char c;
                  declaration lexbuf }

and cdata_section = parse
  | "]]>"       { add_lexeme lexbuf }
  | entity as e { add_entity e;
                  cdata_section lexbuf }
  | _ as c      { add_char c;
                  cdata_section lexbuf }

and link_def acc = parse
  | sp3 '['
      { let f lexbuf =
          let label = link_label lexbuf in
          let destination, title = link_dest lexbuf in
          {Ast.label; destination; title}
        in
        match protect f lexbuf with
        | Ok x ->
            link_def (x :: acc) lexbuf
        | Error lexbuf ->
            acc, lexbuf.Lexing.lex_curr_pos - lexeme_length lexbuf }
  | _ | eof
    { acc, lexbuf.Lexing.lex_curr_pos - lexeme_length lexbuf }

and link_label = parse
  | '\\' (punct as c) { add_char c; link_label lexbuf }
  | ']' ':' { buf () }
  | _ as c { add_char c; link_label lexbuf }

{
let parse defs s =
  let lexbuf = Lexing.from_string s in
  Inline.concat (inline defs [] lexbuf)
}

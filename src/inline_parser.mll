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

let add_entity e =
  match e.[1] with
  | '#' ->
      let num =
        match e.[2] with
        | 'x' | 'X' ->
            Scanf.sscanf e "&#%_[xX]%x;" (fun num -> num)
        | _ ->
            Scanf.sscanf e "&#%d;" (fun num -> num)
      in
      let uch = if num <> 0 && Uchar.is_valid num then Uchar.of_int num else Uchar.rep in
      Buffer.add_utf_8_uchar strbuf uch
  | _ ->
      let name = String.sub e 1 (String.length e - 2) in
      begin match Entities.f name with
      | [] -> add_string e
      | _ :: _ as cps -> List.iter (Buffer.add_utf_8_uchar strbuf) cps
      end

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

let set_lexbuf lexbuf0 lexbuf =
  lexbuf.Lexing.lex_buffer <- lexbuf0.Lexing.lex_buffer;
  lexbuf.Lexing.lex_buffer_len <- lexbuf0.Lexing.lex_buffer_len;
  lexbuf.Lexing.lex_start_pos <- lexbuf0.Lexing.lex_start_pos;
  lexbuf.Lexing.lex_curr_pos <- lexbuf0.Lexing.lex_curr_pos;
  lexbuf.Lexing.lex_last_pos <- lexbuf0.Lexing.lex_last_pos;
  lexbuf.Lexing.lex_last_action <- lexbuf0.Lexing.lex_last_action;
  lexbuf.Lexing.lex_eof_reached <- lexbuf0.Lexing.lex_eof_reached;
  lexbuf.Lexing.lex_mem <- lexbuf0.Lexing.lex_mem;
  lexbuf.Lexing.lex_start_p <- lexbuf0.Lexing.lex_start_p;
  lexbuf.Lexing.lex_curr_p <- lexbuf0.Lexing.lex_curr_p

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

let code_trim s =
  if String.trim s = "" then "" else
  if String.length s >= 2 && s.[0] = ' ' && s.[String.length s - 1] = ' ' then
    String.sub s 1 (String.length s - 2)
  else
    s

let code inline f acc lexbuf =
  let lexbuf0 = copy_lexbuf lexbuf in
  match f lexbuf with
  | () ->
      inline (Pre.R (Code (code_trim (buf ()))) :: acc) lexbuf
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
let nl = '\n' | '\r'
let dig = ['0'-'9']
let hex = dig | ['a'-'f''A'-'F']
let dig8 = dig (dig (dig (dig (dig (dig (dig dig?)?)?)?)?)?)?
let hex8 = hex (hex (hex (hex (hex (hex (hex hex?)?)?)?)?)?)?
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
            ('.'['a'-'z''A'-'Z''0'-'9'](['a'-'z''A'-'Z''0'-'9''-']*['a'-'z''A'-'Z''0'-'9'])?)*
let scheme = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''+''-''.']+
let uri = scheme ':' [^' ''\t''\010'-'\013''<''>''\x00'-'\x1F''\x7F''\x80'-'\x9F']*
let sym_entity = '&' ['a'-'z''A'-'Z''0'-'9']+ ';'
let dec_entity = "&#" dig8 ';'
let hex_entity = "&#" ['x''X'] hex8 ';'
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
                                code (inline defs) (code_span (lexeme_length lexbuf)) acc lexbuf }
  | ('*'+|'_'+ as r)
      { let pre = peek_before ' ' lexbuf |> Pre.classify_delim in
        let post = peek_after ' ' lexbuf |> Pre.classify_delim in
        let e = if r.[0] = '*' then Ast.Star else Underscore in
        let acc = Pre.Emph (pre, post, e, String.length r) :: text acc in
        inline defs acc lexbuf }
  | "!["                      { inline defs (Bang_left_bracket :: text acc) lexbuf }
  | '['                       { inline defs (Left_bracket :: text acc) lexbuf }
  | ']''('                 {
      let acc = text acc in
      let rec loop xs = function
        | Pre.Left_bracket :: acc' ->
           let f lexbuf = Link_parser.destination_and_title_for_reference lexbuf in
           begin match protect f lexbuf with
           | Ok (destination, title) ->
               inline defs (Pre.R (Url {Ast.label = Inline.concat Pre.(List.map to_r (parse_emph xs)); destination; title}) :: acc') lexbuf
           | Error lexbuf ->
               add_lexeme lexbuf; inline defs acc lexbuf
           end
        | x :: acc' -> loop (x :: xs) acc'
        | [] -> add_lexeme lexbuf; inline defs acc lexbuf
      in
      loop [] acc
     }
  | ']' "[]"?
     {
      let acc = text acc in
      let rec loop xs = function
        | Pre.Left_bracket :: acc' ->
           let label = Inline.concat (List.map Pre.to_r (Pre.parse_emph xs)) in
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
  | eof                       { List.map Pre.to_r (Pre.parse_emph (List.rev (text acc))) }

and code_span n = parse
  | '`'+          { if lexeme_length lexbuf <> n then
                      (add_lexeme lexbuf; code_span n lexbuf) }
  | '\n' | '\r'   { add_char ' '; code_span n lexbuf }
  | _ as c        { add_char c; code_span n lexbuf }

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
      { match protect Link_parser.definition lexbuf with
        | Ok x ->
            link_def (x :: acc) lexbuf
        | Error lexbuf ->
            acc, lexbuf.Lexing.lex_curr_pos - lexeme_length lexbuf }
  | _ | eof
    { acc, lexbuf.Lexing.lex_curr_pos - lexeme_length lexbuf }

{
let parse defs s =
  let lexbuf = Lexing.from_string s in
  Inline.concat (inline defs [] lexbuf)
}

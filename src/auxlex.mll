{
type list_item_kind =
  | Ordered of int
  | Bullet of char

let tags =
  [ "address"; "aside"; "base"; "basefont"; "blockquote";
    "body"; "caption"; "center"; "col"; "colgroup"; "dd";
    "details"; "dialog"; "dir"; "div"; "dl"; "dt";
    "fieldset"; "figcaption"; "figure"; "footer"; "form";
    "frame"; "frameset"; "h1"; "h2"; "h3"; "h4"; "h5";
    "h6"; "head"; "header"; "hr"; "html"; "iframe"; "legend";
    "li"; "link"; "main"; "menu"; "menuitem"; "meta"; "nav";
    "noframes"; "ol"; "optgroup"; "option"; "p"; "param";
    "section"; "source"; "summary"; "table"; "tbody";
    "td"; "tfoot"; "th"; "thead"; "title"; "tr"; "track"; "ul" ]
}

let ws = [' ''\t''\n''\r''\011''\012']
let sp3 = (' ' (' ' ' '?)?)?

let unquoted_attribute_value = [^' ''\t''\n''\r''\011''\012''"''\'''=''<''>''`']+
let single_quoted_attribute_value = '\'' [^'\'']* '\''
let double_quoted_attribute_value = '"' [^'"']* '"'
let attribute_value = unquoted_attribute_value | single_quoted_attribute_value | double_quoted_attribute_value
let attribute_value_specification = ws* '=' ws* attribute_value
let attribute_name = ['a'-'z''A'-'Z''_'':']['a'-'z''A'-'Z''0'-'9''_''.'':''-']*
let attribute = ws+ attribute_name attribute_value_specification?
let tag_name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-']*
let open_tag = '<' tag_name attribute+ ws* '/'? '>'
let closing_tag = "</" tag_name ws* '>'

rule is_thematic_break = parse
  | sp3 '*' ws* '*' ws* ('*' ws*)+ eof
  | sp3 '_' ws* '_' ws* ('_' ws*)+ eof
  | sp3 '-' ws* '-' ws* ('-' ws*)+ eof { true }
  | _ | eof { false }

and is_empty = parse
  | ws* eof { true }
  | _ { false }

and is_blockquote = parse
  | sp3 '>' ' '? { Some (String.length (Lexing.lexeme lexbuf)) }
  | _ | eof { None }

and is_list_item = parse
  | sp3 (['+''-''*'] as marker) sp3
      { Some (Bullet marker, String.length (Lexing.lexeme lexbuf)) }
  | sp3 (['0'-'9']+ as num) ('.' | ')') sp3
      { Some (Ordered (int_of_string num), String.length (Lexing.lexeme lexbuf)) }
  | _ | eof
      { None }

and indent acc = parse
  | ' ' { indent (acc + 1) lexbuf }
  | '\t' { indent (acc + 4) lexbuf }
  | _ | eof { acc }

and is_atx_heading = parse
  | sp3 ("#" | "##" | "###" | "####" | "#####" | "######" as atx) (ws _* as title)?
    { let title = match title with None -> "" | Some s -> s in
      let title = String.trim (remove_trailing_hashes (Buffer.create (String.length title)) (Lexing.from_string title)) in
      Some (String.length atx, title) }
  | _ | eof
    { None }

and is_fenced_code = parse
  | (sp3 as ind) ("~~~" '~'* | "```" '`'* as delim) ws* ([^' ''\t']* as info)
      { Some (String.length ind, String.length delim, info) }
  | _ | eof
    { None }

and is_setext_underline = parse
  | sp3 '='+ ws* eof { Some 1 }
  | sp3 '-'+ ws* eof { Some 2 }
  | _ | eof { None }

and is_html_opening = parse
  | sp3 "<!--"
      { Some (true, `Contains ["-->"]) }
  | sp3 "<?"
      { Some (true, `Contains ["?>"]) }
  | sp3 "<!"
      { Some (true, `Contains [">"]) }
  | sp3 "<![CDATA["
      { Some (true, `Contains ["]]>"]) }
  | sp3 ("<script" | "<pre" | "<style") (ws+ | '>' | eof)
      { Some (true, `Contains ["</script>"; "</pre>"; "</style>"]) }
  | sp3 ('<' | "</") (tag_name as tag) (ws+ | eof | '>' | "/>")
      { if List.mem (String.lowercase_ascii tag) tags then
          Some (true, `Blank)
        else
          None }
  | sp3 (open_tag | closing_tag) ws* eof
      { Some (false, `Blank) }
  | _ | eof
      { None }

and remove_trailing_hashes b = parse
  | ' ' '#'+ ws* eof | eof { Buffer.contents b }
  | _ as c { Buffer.add_char b c; remove_trailing_hashes b lexbuf }
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

let is_html_opening s =
  is_html_opening (Lexing.from_string s)

let is_indented_code s =
  indent s >= 4

let is_setext_underline s =
  is_setext_underline (Lexing.from_string s)
}

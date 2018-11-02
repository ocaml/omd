{
let strbuf =
  Buffer.create 101

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

let get_buf () =
  let s = Buffer.contents strbuf in
  Buffer.clear strbuf;
  s

let lexeme_length lexbuf =
  Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf

type html_kind =
  | Hcontains of string list
  | Hblank

module R = struct
  type t =
    | Lempty
    | Lblockquote of int
    | Lthematic_break
    | Latx_heading of int * string
    | Lsetext_heading of int * int
    | Lfenced_code of int * int * Ast.fenced_code_kind * string
    | Lhtml of bool * html_kind
    | Llist_item of Ast.list_kind * int
    | Lparagraph
end

type line_kind =
  | Lempty
  | Lblockquote of Sub.t
  | Lthematic_break
  | Latx_heading of int * string
  | Lsetext_heading of int * int
  | Lfenced_code of int * int * Ast.fenced_code_kind * string
  | Lindented_code of Sub.t
  | Lhtml of bool * html_kind
  | Llist_item of Ast.list_kind * int * Sub.t
  | Lparagraph

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
let dig = ['0'-'9']
let hex = dig | ['a'-'f''A'-'F']
let dig8 = dig (dig (dig (dig (dig (dig (dig dig?)?)?)?)?)?)?
let hex8 = hex (hex (hex (hex (hex (hex (hex hex?)?)?)?)?)?)?

let punct = ['!''"''#''$''%''&''\'''('')''*''+'',''-''.''/'':'';''<''=''>''?''@''[''\\'']''^''_''`''{''|''}''~']

let unquoted_attribute_value = [^' ''\t''\n''\r''\011''\012''"''\'''=''<''>''`']+
let single_quoted_attribute_value = '\'' [^'\'']* '\''
let double_quoted_attribute_value = '"' [^'"']* '"'
let attribute_value = unquoted_attribute_value | single_quoted_attribute_value | double_quoted_attribute_value
let attribute_value_specification = ws* '=' ws* attribute_value
let attribute_name = ['a'-'z''A'-'Z''_'':']['a'-'z''A'-'Z''0'-'9''_''.'':''-']*
let attribute = ws+ attribute_name attribute_value_specification?
let tag_name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-']*
let list_item_num =
  dig (dig (dig (dig (dig (dig (dig (dig dig?)?)?)?)?)?)?)?
let sym_entity = '&' ['a'-'z''A'-'Z''0'-'9']+ ';'
let dec_entity = "&#" dig8 ';'
let hex_entity = "&#" ['x''X'] hex8 ';'
let entity = sym_entity | dec_entity | hex_entity

rule line = parse
  | ws* eof { R.Lempty }
  | sp3 '>' { Lblockquote (lexeme_length lexbuf) }
  | sp3 ('='+ as s) ws* eof { Lsetext_heading (1, String.length s) }
  | sp3 ('-' as marker) as l ws* eof  { Llist_item (Ast.Unordered marker, String.length l) }
  | sp3 ('-'+ as s) ws* eof { Lsetext_heading (2, String.length s) }
  | sp3 '*' ws* '*' (ws* '*')+ ws* eof
  | sp3 '_' ws* '_' (ws* '_')+ ws* eof
  | sp3 '-' ws* '-' (ws* '-')+ ws* eof { Lthematic_break }
  | sp3 ("#" | "##" | "###" | "####" | "#####" | "######" as atx) (ws _* as title)? eof
    { let title = match title with None -> "" | Some s -> s in
      let title = String.trim (remove_trailing_hashes (Buffer.create (String.length title)) (Lexing.from_string title)) in
      Latx_heading (String.length atx, title) }
  | (sp3 as ind) ("```" '`'* as delim) ws* ([^'`'' ''\t''\010'-'\013']* as info) [^'`']* eof
      { let info = info_string (Lexing.from_string info) in
        Lfenced_code (String.length ind, String.length delim, Backtick, info) }
  | (sp3 as ind) ("~~~" '~'* as delim) ws* ([^' ''\t''\010'-'\013']* as info)
      { let info = info_string (Lexing.from_string info) in
        Lfenced_code (String.length ind, String.length delim, Tilde, info) }
  | sp3 '<'
      { match html0 lexbuf with (p, stop) -> Lhtml (p, stop) | exception _ -> Lparagraph }
  | sp3 (['+''-''*'] as marker) as l ([' ''\t'] | eof)
      { Llist_item (Ast.Unordered marker, String.length l) }
  | sp3 (list_item_num as num) ('.' | ')' as d) as l ([' ''\t'] | eof)
      { Llist_item (Ast.Ordered (int_of_string num, d), String.length l) }
  | _
      { Lparagraph }

and info_string = parse
  | '\\' (punct as c) { add_char c; info_string lexbuf }
  | entity as e       { add_entity e; info_string lexbuf }
  | _ as c            { add_char c; info_string lexbuf }
  | eof               { get_buf () }

and is_empty = parse
  | ws* eof { true }
  | _ { false }

and count_indent acc = parse
  | ' ' { count_indent (acc + 1) lexbuf }
  | '\t' { count_indent (acc + 4) lexbuf }
  | _ | eof { acc }

and html0 = parse
  | '?'                                           { (true, Hcontains ["?>"]) }
  | "!--"                                         { (true, Hcontains ["-->"]) }
  | '!'                                           { (true, Hcontains [">"]) }
  | "![CDATA["                                    { (true, Hcontains ["]]>"]) }
  | '/' (tag_name as t)                           { html_closing t lexbuf }
  | ("script" | "pre" | "style") (ws | '>' | eof) { (true, Hcontains ["</script>"; "</pre>"; "</style>"]) }
  | tag_name as t                                 { html_open t lexbuf }

and html_open t = parse
  | ws | eof | '/'? '>'                           { if List.mem (String.lowercase_ascii t) tags then (true, Hblank) else raise Exit }
  | (attribute* as a) ws* '/'? '>' ws* eof        { if a = "" && List.mem (String.lowercase_ascii t) tags then (true, Hblank) else (false, Hblank) }

and html_closing t = parse
  | ws* '>' ws* eof                               { (false, Hblank) }
  | ws | eof | '>'                                { if List.mem (String.lowercase_ascii t) tags then (true, Hblank) else raise Exit }

and remove_trailing_hashes b = parse
  | ' ' '#'+ ws* eof | eof { Buffer.contents b }
  | _ as c { Buffer.add_char b c; remove_trailing_hashes b lexbuf }

{
let is_empty s =
  is_empty (Sub.lexbuf s)

let indent s =
  count_indent 0 (Sub.lexbuf s)

let is_indented_code s =
  indent s >= 4

let classify_line s =
  match line (Sub.lexbuf s) with
  | R.Lblockquote n ->
      let s = Sub.offset n s in
      let s = if indent s > 0 then Sub.offset 1 s else s in
      Lblockquote s
  | Lparagraph ->
      if indent s >= 4 then
        Lindented_code (Sub.offset 4 s)
      else
        Lparagraph
  | Llist_item (kind, off) ->
      let off =
        let s = Sub.offset off s in
        let n = indent s in
        (* if n > 0 then *)
          if 0 < n && n <= 4 && not (is_empty s) then
            off + n
          else
            off + 1
      in
      Llist_item (kind, off, Sub.offset off s)
  | Lempty -> Lempty
  | Lthematic_break -> Lthematic_break
  | Latx_heading (n, s) -> Latx_heading (n, s)
  | Lsetext_heading (n, m) -> Lsetext_heading (n, m)
  | Lfenced_code (a, b, k, s) -> Lfenced_code (a, b, k, s)
  | Lhtml (p, k) -> Lhtml (p, k)
}

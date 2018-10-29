{
let lexeme_length lexbuf =
  Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf

type list_kind =
  | Ordered
  | Unordered

type html_kind =
  | Hcontains of string list
  | Hblank

type fenced_code_kind =
  | Tilde
  | Backtick

module R = struct
  type list_item_kind =
    | Ordered of int
    | Bullet of char

  type t =
    | Lempty
    | Lblockquote of int
    | Lthematic_break
    | Latx_heading of int * string
    | Lsetext_heading of int * int
    | Lfenced_code of int * int * fenced_code_kind * string
    | Lhtml of bool * html_kind
    | Llist_item of list_item_kind * int
    | Lparagraph
end

type line_kind =
  | Lempty
  | Lblockquote of Sub.t
  | Lthematic_break
  | Latx_heading of int * string
  | Lsetext_heading of int * int
  | Lfenced_code of int * int * fenced_code_kind * string
  | Lindented_code of Sub.t
  | Lhtml of bool * html_kind
  | Llist_item of list_kind * int * Sub.t
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

let unquoted_attribute_value = [^' ''\t''\n''\r''\011''\012''"''\'''=''<''>''`']+
let single_quoted_attribute_value = '\'' [^'\'']* '\''
let double_quoted_attribute_value = '"' [^'"']* '"'
let attribute_value = unquoted_attribute_value | single_quoted_attribute_value | double_quoted_attribute_value
let attribute_value_specification = ws* '=' ws* attribute_value
let attribute_name = ['a'-'z''A'-'Z''_'':']['a'-'z''A'-'Z''0'-'9''_''.'':''-']*
let attribute = ws+ attribute_name attribute_value_specification?
let tag_name = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-']*

rule line = parse
  | ws* eof { R.Lempty }
  | sp3 '>' { Lblockquote (lexeme_length lexbuf) }
  | sp3 ('='+ as s) ws* eof { Lsetext_heading (1, String.length s) }
  | sp3 ('-'+ as s) ws* eof { Lsetext_heading (2, String.length s) }
  | sp3 '*' ws* '*' (ws* '*')+ ws* eof
  | sp3 '_' ws* '_' (ws* '_')+ ws* eof
  | sp3 '-' ws* '-' (ws* '-')+ ws* eof { Lthematic_break }
  | sp3 ("#" | "##" | "###" | "####" | "#####" | "######" as atx) (ws _* as title)? eof
    { let title = match title with None -> "" | Some s -> s in
      let title = String.trim (remove_trailing_hashes (Buffer.create (String.length title)) (Lexing.from_string title)) in
      Latx_heading (String.length atx, title) }
  | (sp3 as ind) ("```" '`'* as delim) ws* ([^'`'' ''\t''\010'-'\013']* as info) [^'`']* eof
      { Lfenced_code (String.length ind, String.length delim, Backtick, String.trim info) }
  | (sp3 as ind) ("~~~" '~'* as delim) ws* ([^' ''\t''\010'-'\013']* as info)
      { Lfenced_code (String.length ind, String.length delim, Tilde, String.trim info) }
  | sp3 '<'
      { match html0 lexbuf with (p, stop) -> Lhtml (p, stop) | exception _ -> Lparagraph }
  | sp3 (['+''-''*'] as marker) as l [' ''\t']
      { Llist_item (R.Bullet marker, String.length l) }
  | sp3 (['0'-'9']+ as num) ('.' | ')') as l [' ''\t']
      { Llist_item (R.Ordered (int_of_string num), String.length l) }
  | _
      { Lparagraph }

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
        let n = indent (Sub.offset off s) in
        (* if n > 0 then *)
          if n <= 4 then
            off + n
          else
            off + 1
      in
      let kind =
        match kind with
        | Bullet _ -> Unordered
        | Ordered _ -> Ordered
      in
      Llist_item (kind, off, Sub.offset off s)
  | Lempty -> Lempty
  | Lthematic_break -> Lthematic_break
  | Latx_heading (n, s) -> Latx_heading (n, s)
  | Lsetext_heading (n, m) -> Lsetext_heading (n, m)
  | Lfenced_code (a, b, k, s) -> Lfenced_code (a, b, k, s)
  | Lhtml (p, k) -> Lhtml (p, k)
}

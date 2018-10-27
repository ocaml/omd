{
type list_item_kind =
  | Ordered of int
  | Bullet of char

type fenced_code_kind =
  | Tilde
  | Backtick

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
  | sp3 '*' ws* '*' (ws* '*')+ ws* eof
  | sp3 '_' ws* '_' (ws* '_')+ ws* eof
  | sp3 '-' ws* '-' (ws* '-')+ ws* eof { true }
  | _ | eof { false }

and is_empty = parse
  | ws* eof { true }
  | _ { false }

and is_blockquote = parse
  | sp3 '>' { Some (String.length (Lexing.lexeme lexbuf)) }
  | _ | eof { None }

and is_list_item = parse
  | sp3 (['+''-''*'] as marker)
      { Some (Bullet marker, String.length (Lexing.lexeme lexbuf)) }
  | sp3 (['0'-'9']+ as num) ('.' | ')')
      { Some (Ordered (int_of_string num), String.length (Lexing.lexeme lexbuf)) }
  | _ | eof
      { None }

and indent acc = parse
  | ' ' { indent (acc + 1) lexbuf }
  | '\t' { indent (acc + 4) lexbuf }
  | _ | eof { acc }

and is_atx_heading = parse
  | sp3 ("#" | "##" | "###" | "####" | "#####" | "######" as atx) (ws _* as title)? eof
    { let title = match title with None -> "" | Some s -> s in
      let title = String.trim (remove_trailing_hashes (Buffer.create (String.length title)) (Lexing.from_string title)) in
      Some (String.length atx, title) }
  | _ | eof
    { None }

and is_fenced_code = parse
  | (sp3 as ind) ("~~~" '~'* | "```" '`'* as delim) ([^'`']* as info) eof
      { let q = if delim.[0] = '~' then Tilde else Backtick in
        Some (String.length ind, String.length delim, q, String.trim info) }
  | _ | eof
    { None }

and is_setext_underline = parse
  | sp3 ('='+ as s) ws* eof { Some (1, String.length s) }
  | sp3 ('-'+ as s) ws* eof { Some (2, String.length s) }
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
  | sp3 ("<script" | "<pre" | "<style") (ws | '>' | eof)
      { Some (true, `Contains ["</script>"; "</pre>"; "</style>"]) }
  | sp3 ('<' | "</") (tag_name as tag) (ws | eof | '>' | "/>")
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
  is_thematic_break (Sub.lexbuf s)

let is_empty s =
  is_empty (Sub.lexbuf s)

let is_blockquote s =
  is_blockquote (Sub.lexbuf s)

let indent s =
  indent 0 (Sub.lexbuf s)

let is_list_item s =
  match is_list_item (Sub.lexbuf s) with
  | Some (k, off) ->
      let n = indent (Sub.offset off s) in
      if n > 0 then
        if n <= 4 then
          Some (k, off + n)
        else
          Some (k, off + 1)
      else
        None
  | None ->
      None

let is_atx_heading s =
  is_atx_heading (Sub.lexbuf s)

let is_fenced_code s =
  is_fenced_code (Sub.lexbuf s)

let is_fenced_code_closing num q s =
  match is_fenced_code s with
  | Some (_, num', q1, "") ->
      num' >= num && q = q1
  | _ ->
      false

let is_html_opening s =
  is_html_opening (Sub.lexbuf s)

let is_indented_code s =
  indent s >= 4

let is_setext_underline s =
  is_setext_underline (Sub.lexbuf s)

type list_kind =
  | Ordered
  | Unordered

type html_kind =
  | Hcontains of string list
  | Hblank

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
  | Lparagraph of Sub.t

let classify_line (s : Sub.t) =
  if is_empty s then
    Lempty
  else begin
    match is_blockquote s with
    | Some n ->
        let s = Sub.offset n s in
        let s = if indent s > 0 then Sub.offset 1 s else s in
        Lblockquote s
    | None ->
        begin match is_setext_underline s with
        | Some (n, l) ->
            Lsetext_heading (n, l)
        | None ->
            begin match is_thematic_break s with
            | true ->
                Lthematic_break
            | false ->
                begin match is_atx_heading s with
                | Some (n, s) ->
                    Latx_heading (n, s)
                | None ->
                    begin match is_fenced_code s with
                    | Some (ind, num, q, info) ->
                        Lfenced_code (ind, num, q, info)
                    | None ->
                        begin match is_html_opening s with
                        | Some (can_interrupt_par, kind) ->
                            let kind =
                              match kind with
                              | `Contains l -> Hcontains l
                              | `Blank -> Hblank
                            in
                            Lhtml (can_interrupt_par, kind)
                        | None ->
                            if indent s >= 4 then
                              Lindented_code (Sub.offset 4 s)
                            else begin
                              match is_list_item s with
                              | Some (kind, indent) ->
                                  let kind =
                                    match kind with
                                    | Bullet _ -> Unordered
                                    | Ordered _ -> Ordered
                                  in
                                  Llist_item (kind, indent, Sub.offset indent s)
                              | None ->
                                  Lparagraph s
                            end
                        end
                    end
                end
            end
        end
  end

}

open Ast

type html_kind =
  | Hcontains of string list
  | Hblank

type t =
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

let sp3 s =
  match Sub.head s with
  | Some ' ' ->
      let s = Sub.tail s in
      begin match Sub.head s with
      | Some ' ' ->
          let s = Sub.tail s in
          begin match Sub.head s with
          | Some ' ' -> 3, Sub.tail s
          | Some _ | None -> 2, s
          end
      | Some _ | None -> 1, s
      end
  | Some _ | None -> 0, s

exception Fail

let (|||) p1 p2 s =
  try p1 s with Fail -> p2 s

let rec ws ?rev s =
  match Sub.head ?rev s with
  | Some (' ' | '\t' | '\010'..'\013') -> ws ?rev (Sub.tail ?rev s)
  | None | Some _ -> s

let is_empty s =
  Sub.is_empty (ws s)

let thematic_break s =
  match Sub.head s with
  | Some ('*' | '_' | '-' as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 ->
            loop (succ n) (Sub.tail s)
        | Some (' ' | '\t' | '\010'..'\013') ->
            loop n (Sub.tail s)
        | Some _ ->
            raise Fail
        | None ->
            if n < 3 then raise Fail;
            Lthematic_break
      in
      loop 1 (Sub.tail s)
  | Some _ | None ->
      raise Fail

let setext_heading s =
  match Sub.head s with
  | Some ('-' | '=' as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 ->
            loop (succ n) (Sub.tail s)
        | Some _ | None ->
            if not (Sub.is_empty (ws s)) then raise Fail;
            if c = '-' && n = 1 then raise Fail; (* can be interpreted as an empty list item *)
            Lsetext_heading ((if c = '-' then 2 else 1), n)
      in
      loop 1 (Sub.tail s)
  | Some _ | None ->
      raise Fail

let atx_heading s =
  let rec loop n s =
    if n > 6 then raise Fail;
    match Sub.head s with
    | Some '#' ->
        loop (succ n) (Sub.tail s)
    | Some (' ' | '\t' | '\010'..'\013') ->
        let s = ws ~rev:() (ws s) in
        let rec loop t =
          match Sub.head ~rev:() t with
          | Some '#' ->
              loop (Sub.tail ~rev:() t)
          | Some (' ' | '\t' | '\010'..'\013') | None ->
              ws ~rev:() t
          | Some _ ->
              s
        in
        Latx_heading (n, Sub.to_string (loop s))
    | Some _ ->
        raise Fail
    | None ->
        Latx_heading (n, Sub.to_string s)
  in
  loop 0 s

let is_punct = function
  | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')'
  | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<'
  | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_'
  | '`' | '{' | '|' | '}' | '~' -> true
  | _ -> false

let entity s =
  match Sub.heads 2 s with
  | '#' :: ('x' | 'X') :: _ ->
      let rec loop m n s =
        if m > 8 then raise Fail;
        match Sub.head s with
        | Some ('a'..'f' as c) ->
            loop (succ m) (n * 16 + Char.code c - Char.code 'a' + 10) (Sub.tail s)
        | Some ('A'..'F' as c) ->
            loop (succ m) (n * 16 + Char.code c - Char.code 'A' + 10) (Sub.tail s)
        | Some ('0'..'9' as c) ->
            loop (succ m) (n * 16 + Char.code c - Char.code '0') (Sub.tail s)
        | Some ';' ->
            if m = 0 then raise Fail;
            let u = if n = 0 || not (Uchar.is_valid n) then Uchar.rep else Uchar.of_int n in
            [u], Sub.tail s
        | Some _ | None ->
            raise Fail
      in
      loop 0 0 (Sub.tails 2 s)
  | '#' :: _ ->
      let rec loop m n s =
        if m > 8 then raise Fail;
        match Sub.head s with
        | Some ('0'..'9' as c) ->
            loop (succ m) (n * 10 + Char.code c - Char.code '0') (Sub.tail s)
        | Some ';' ->
            if m = 0 then raise Fail;
            let u = if n = 0 || not (Uchar.is_valid n) then Uchar.rep else Uchar.of_int n in
            [u], Sub.tail s
        | Some _ | None ->
            raise Fail
      in
      loop 0 0 (Sub.tail s)
  | ('a'..'z' | 'A'..'Z') :: _ ->
      let rec loop len t =
        match Sub.head t with
        | Some ('a'..'z' | 'A'..'Z' | '0'..'9') ->
            loop (succ len) (Sub.tail t)
        | Some ';' ->
            let name = Sub.to_string (Sub.sub ~len s) in
            begin match Entities.f name with
            | [] -> raise Fail
            | cps -> cps, Sub.tail t
            end
        | Some _ | None ->
            raise Fail
      in
      loop 1 (Sub.tail s)
  | _ ->
      raise Fail

let info_string c s =
  let buf = Buffer.create 17 in
  let rec loop s =
    match Sub.head s with
    | Some (' ' | '\t' | '\010'..'\013') | None ->
        if c = '`' && Sub.exists (function '`' -> true | _ -> false) s then raise Fail;
        Buffer.contents buf
    | Some '`' when c = '`' ->
        raise Fail
    | Some ('\\' as c) ->
        let s = Sub.tail s in
        begin match Sub.head s with
        | Some c when is_punct c ->
            Buffer.add_char buf c;
            loop (Sub.tail s)
        | Some _ | None ->
            Buffer.add_char buf c;
            loop s
        end
    | Some ('&' as c) ->
        let s = Sub.tail s in
        begin match entity s with
        | (ul, s) ->
            List.iter (Buffer.add_utf_8_uchar buf) ul;
            loop s
        | exception Fail ->
            Buffer.add_char buf c;
            loop s
        end
    | Some c ->
        Buffer.add_char buf c;
        loop (Sub.tail s)
  in
  loop (ws s)

let fenced_code ind s =
  match Sub.head s with
  | Some ('`' | '~' as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 ->
            loop (succ n) (Sub.tail s)
        | Some _ | None ->
            if n < 3 then raise Fail;
            let s = info_string c s in
            let c = if c = '`' then Backtick else Tilde in
            Lfenced_code (ind, n, c, s)
      in
      loop 1 (Sub.tail s)
  | Some _ | None ->
      raise Fail

let indent s =
  let rec loop n s =
    match Sub.head s with
    | Some ' ' ->
        loop (n + 1) (Sub.tail s)
    | Some '\t' ->
        loop (n + 4) (Sub.tail s)
    | Some _ | None ->
        n
  in
  loop 0 s

let unordered_list_item ind s =
  match Sub.head s with
  | Some ('+' | '-' | '*' as c) ->
      let s = Sub.tail s in
      if is_empty s then
        Llist_item (Unordered c, 2 + ind, s)
      else
        let n = indent s in
        if n = 0 then raise Fail;
        let n = if n <= 4 then n else 1 in
        Llist_item (Unordered c, n + 1 + ind, Sub.offset n s)
  | Some _ | None ->
      raise Fail

let ordered_list_item ind s =
  let rec loop n m s =
    match Sub.head s with
    | Some ('0'..'9' as c) ->
        if n >= 9 then raise Fail;
        loop (succ n) (m * 10 + Char.code c - Char.code '0') (Sub.tail s)
    | Some ('.' | ')' as c) ->
        let s = Sub.tail s in
        if is_empty s then
          Llist_item (Ordered (m, c), n + 1 + ind, s)
        else begin
          let ind' = indent s in
          if ind' = 0 then raise Fail;
          let ind' = if ind' <= 4 then ind' else 1 in
          Llist_item (Ordered (m, c), n + ind + ind' + 1, Sub.offset ind' s)
        end
    | Some _ | None ->
        raise Fail
  in
  loop 0 0 s

let tag_name s0 =
  match Sub.head s0 with
  | Some ('a'..'z' | 'A'..'Z') ->
      let rec loop len s =
        match Sub.head s with
        | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '-') ->
            loop (succ len) (Sub.tail s)
        | Some _ | None ->
            Sub.to_string (Sub.sub s0 ~len), s
      in
      loop 1 (Sub.tail s0)
  | Some _ | None ->
      raise Fail

let known_tags =
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

let special_tags =
  [ "script"; "pre"; "style" ]

let known_tag s =
  let s = String.lowercase_ascii s in
  List.mem s known_tags

let special_tag s =
  let s = String.lowercase_ascii s in
  List.mem s special_tags

let closing_tag s =
  let s = ws s in
  match Sub.head s with
  | Some '>' ->
      if not (is_empty (Sub.tail s)) then raise Fail;
      Lhtml (false, Hblank)
  | Some _ | None ->
      raise Fail

let special_tag tag s =
  if not (special_tag tag) then raise Fail;
  match Sub.head s with
  | Some (' ' | '\t' | '\010'..'\013' | '>') | None ->
      Lhtml (true, Hcontains ["</script>"; "</pre>"; "</style>"])
  | Some _ ->
      raise Fail

let known_tag tag s =
  if not (known_tag tag) then raise Fail;
  match Sub.heads 2 s with
  | (' ' | '\t' | '\010'..'\013') :: _
  | [] | '>' :: _ | '/' :: '>' :: _ -> Lhtml (true, Hblank)
  | _ -> raise Fail

let ws1 s =
  match Sub.head s with
  | Some (' ' | '\t' | '\010'..'\013') ->
      ws s
  | Some _ | None ->
      raise Fail

let attribute_name s =
  match Sub.head s with
  | Some ('a'..'z' | 'A'..'Z' | '_' | ':') ->
      let rec loop s =
        match Sub.head s with
        | Some ('a'..'z' | 'A'..'Z' | '_' | '.' | ':' | '0'..'9') ->
            loop (Sub.tail s)
        | Some _ | None ->
            s
      in
      loop s
  | Some _ | None ->
      raise Fail

let attribute_value s =
  match Sub.head s with
  | Some ('\'' | '"' as c) ->
      let rec loop s =
        match Sub.head s with
        | Some c1 when c = c1 ->
            Sub.tail s
        | Some _ ->
            loop (Sub.tail s)
        | None ->
            raise Fail
      in
      loop (Sub.tail s)
  | Some _ ->
      let rec loop first s =
        match Sub.head s with
        | Some (' ' | '\t' | '\010'..'\013' | '"' | '\'' | '=' | '<' | '>' | '`')
        | None ->
            if first then raise Fail;
            s
        | Some _ ->
            loop false (Sub.tail s)
      in
      loop true s
  | None ->
      raise Fail

let attribute s =
  let s = ws1 s in
  let s = attribute_name s in
  let s = ws s in
  match Sub.head s with
  | Some '=' ->
      let s = ws (Sub.tail s) in
      attribute_value s
  | Some _ | None ->
      s

let attributes s =
  let rec loop s =
    match attribute s with
    | s -> loop s
    | exception Fail -> s
  in
  loop s

let open_tag s =
  let s = attributes s in
  let s = ws s in
  let n =
    match Sub.heads 2 s with
    | '/' :: '>' :: _ -> 2
    | '>' :: _ -> 1
    | _ -> raise Fail
  in
  if not (is_empty (Sub.tails n s)) then raise Fail;
  Lhtml (false, Hblank)

let raw_html s =
  match Sub.heads 10 s with
  | '<' :: '?' :: _ ->
      Lhtml (true, Hcontains ["?>"])
  | '<' :: '!' :: '-' :: '-' :: _ ->
      Lhtml (true, Hcontains ["-->"])
  | '<' :: '!' :: '[' :: 'C' :: 'D' :: 'A' :: 'T' :: 'A' :: '[' :: _ ->
      Lhtml (true, Hcontains ["]]>"])
  | '<' :: '!' :: _ ->
      Lhtml (true, Hcontains [">"])
  | '<' :: '/' :: _ ->
      let tag, s = tag_name (Sub.tails 2 s) in
      (known_tag tag ||| closing_tag) s
  | '<' :: _ ->
      let tag, s = tag_name (Sub.tails 1 s) in
      (special_tag tag ||| known_tag tag ||| open_tag) s
  | _ ->
      raise Fail

let blank s =
  if not (is_empty s) then raise Fail;
  Lempty

let indented_code ind s =
  if indent s + ind < 4 then raise Fail;
  Lindented_code (Sub.offset (4 - ind) s)

let parse s0 =
  let ind, s = sp3 s0 in
  match Sub.head s with
  | Some '>' ->
      let s = Sub.offset 1 s in
      let s = if indent s > 0 then Sub.offset 1 s else s in
      Lblockquote s
  | Some '=' ->
      setext_heading s
  | Some '-' ->
      (setext_heading ||| thematic_break ||| unordered_list_item ind) s
  | Some ('_') ->
      thematic_break s
  | Some '#' ->
      atx_heading s
  | Some ('~' | '`') ->
      fenced_code ind s
  | Some '<' ->
      raw_html s
  | Some '*' ->
      (thematic_break ||| unordered_list_item ind) s
  | Some '+' ->
      unordered_list_item ind s
  | Some ('0'..'9') ->
      ordered_list_item ind s
  | Some _ ->
      (blank ||| indented_code ind) s
  | None ->
      Lempty

let parse s0 =
  try parse s0 with Fail -> Lparagraph

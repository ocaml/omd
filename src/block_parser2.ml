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

module P : sig
  type state
  type 'a t = state -> 'a

  exception Fail

  val peek: char t
  val advance: int -> unit t
  val char: char -> unit t
  val next: char t
  val accumulate: char option t -> string t
  val try_bind: 'a t -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val fail: 'a t
  val return: 'a -> 'a t
  val (|||): 'a t -> 'a t -> 'a t
  val ws: unit t
  val ws1: unit t
  val (>>>): unit t -> 'a t -> 'a t
  val (<<<): 'a t -> unit t -> 'a t

  val copy_state: state -> state
end = struct
  type state =
    {
      mutable str: string;
      mutable pos: int;
    }

  let copy_state {str; pos} =
    {str; pos}

  let restore_state st {str; pos} =
    st.str <- str;
    st.pos <- pos

  type 'a t =
    state -> 'a

  exception Fail

  let char c st =
    if st.pos >= String.length st.str then raise Fail;
    if st.str.[st.pos] <> c then raise Fail

  let next st =
    if st.pos >= String.length st.str then
      raise Fail
    else
      let c = st.str.[st.pos] in (st.pos <- succ st.pos; c)

  let peek st =
    if st.pos >= String.length st.str then
      raise Fail
    else
      st.str.[st.pos]

  let advance n st =
    assert (n >= 0);
    let n = min (String.length st.str - st.pos) n in
    st.pos <- st.pos + n

  let return c _ = c
  let (>>=) p f st = f (p st) st

  let accumulate p =
    let b = Buffer.create 17 in
    let rec loop () =
      p >>= function
      | Some c -> Buffer.add_char b c; loop ()
      | None -> return (Buffer.contents b)
    in
    loop ()

  let protect p st =
    let st' = copy_state st in
    try p st with e -> restore_state st st'; raise e

  let try_bind p f q st =
    match protect p st with
    | r -> f r st
    | exception e -> q e st

  let fail _ =
    raise Fail

  let (|||) p1 p2 st =
    try protect p1 st with Fail -> p2 st

  let rec ws st =
    match next st with
    | ' ' | '\t' | '\010'..'\013' -> ws st
    | _ -> ()
    | exception _ -> ()

  let ws1 st =
    match next st with
    | ' ' | '\t' | '\010'..'\013' -> ws st
    | _ -> fail st

  let (>>>) p q st =
    p st; q st

  let (<<<) p q st =
    let x = p st in
    q st; x
end

open P

let is_empty st =
  let st = copy_state st in
  try
    let rec loop () =
      match next st with
      | ' ' | '\t' | '\010'..'\013' -> loop ()
      | _ -> false
    in
    loop ()
  with Fail ->
    true

module Pre = Inline.Pre

let inline defs st =
  let buf = Buffer.create 0 in
  let get_buf () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in
  let text acc =
    if Buffer.length buf = 0 then
      acc
    else
      Pre.R (Text (get_buf ())) :: acc
  in
  let rec loop acc st =
    match peek st with
    | '<' ->
        assert false
    | ' ' as c ->
        let st0 = copy_state st in
        begin match (ws1 >>> char '\n' >>> ws) st with
        | () ->
            loop (Pre.R Hard_break :: text acc) st
        | exception Fail ->
            Buffer.add_char buf c; loop acc st0
        end
    | '\\' ->
        advance 1 st;
        begin match next st with
        | '\n' ->
            loop (Pre.R Hard_break :: text acc) st
        | c when is_punct c ->
            Buffer.add_char buf c; loop acc st
        | c ->
            Buffer.add_char buf '\\'; Buffer.add_char buf c; loop acc st
        | exception End_of_file ->
            Buffer.add_char buf '\\'; loop acc st
        end
    | '!' as c ->
        advance 1 st;
        begin match peek st with
        | '[' ->
            advance 1 st; loop (Bang_left_bracket :: text acc) st
        | _ ->
            Buffer.add_char buf c; loop acc st
        | exception Fail ->
            Buffer.add_char buf c; loop acc st
        end
    | '&' as c ->
        advance 1 st;
        begin match peek st with
        | '#' ->
            advance 1 st;
            begin match peek st with
            | 'x' | 'X' ->
                let rec aux n m =
                  match peek st with (* FIXME max 8 chars *)
                  | '0'..'9' ->
                      advance 1 st;
                      aux (succ n) (m * 16 + Char.code c - Char.code '0')
                  | 'a'..'f' ->
                      advance 1 st;
                      aux (succ n) (m * 16 + Char.code c - Char.code 'a' + 10)
                  | 'A'..'F' ->
                      advance 1 st;
                      aux (succ n) (m * 16 + Char.code c - Char.code 'A' + 10)
                  | ';' ->
                      advance 1 st;
                      let u = if Uchar.is_valid m && m <> 0 then Uchar.of_int m else Uchar.rep in
                      Buffer.add_utf_8_uchar buf u;
                      loop acc st
                  | _ ->
                      assert false (* add partial lexeme *)
                  | exception Fail ->
                      assert false (* add partial lexeme *)
                in
                aux 0 0
            | '0'..'9' ->
                let rec aux n m =
                  match peek st with (* FIXME max 8 chars *)
                  | '0'..'9' ->
                      advance 1 st;
                      aux (succ n) (m * 10 + Char.code c - Char.code '0')
                  | ';' ->
                      advance 1 st;
                      let u = if Uchar.is_valid m && m <> 0 then Uchar.of_int m else Uchar.rep in
                      Buffer.add_utf_8_uchar buf u;
                      loop acc st
                  | _ ->
                      assert false (* add partial lexeme *)
                  | exception Fail ->
                      assert false (* add partial lexeme *)
                in
                aux 0 0
            | _ ->
                Buffer.add_string buf "&#"; loop acc st
            | exception Fail ->
                Buffer.add_string buf "&#"; loop acc st
            end
        | '0'..'9' | 'a'..'z' | 'A'..'9' ->
            let b = Buffer.create 7 in
            let rec aux () =
              match peek st with
              | '0'..'9' | 'a'..'z' | 'A'..'Z' as c ->
                  advance 1 st; Buffer.add_char b c; aux ()
              | ';' ->
                  advance 1 st;
                  let name = Buffer.contents b in
                  begin match Entities.f name with
                  | [] -> (* add partial lexeme *) assert false
                  | _ :: _ as cps -> List.iter (Buffer.add_utf_8_uchar buf) cps
                  end;
                  loop acc st
              | _ ->
                  assert false (* add partial lexeme *)
              | exception Fail ->
                  assert false (* add partial lexeme *)
            in
            aux ()
        | _ ->
            Buffer.add_char buf c; loop acc st
        | exception Fail ->
            Buffer.add_char buf c; loop acc st
        end
    | '[' ->
        advance 1 st; loop (Left_bracket :: text acc) st
    | ']' ->
        advance 1 st;
        begin match peek st with
        | '(' ->
            ...
        end
    | '*' | '_' as c ->
        let pre = peek_before ' ' st |> Pre.classify_delim in
        let f post n st =
          let post = post |> Pre.classify_delim in
          let e = if c = '*' then Ast.Star else Underscore in
          loop (Pre.Emph (pre, post, e, n) :: text acc) st
        in
        let rec aux n =
          match peek st with
          | c1 when c1 = c -> advance 1 st; aux (succ n)
          | c1 -> f c1 n st
          | exception End_of_file -> f ' ' n st
        in
        aux 0
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop acc st
    | exception Fail ->
        Pre.parse_emph (List.rev (text acc))
  in
  loop [] st

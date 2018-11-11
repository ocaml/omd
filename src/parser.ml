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

  val of_string: string -> state
  val push_mark: state -> unit
  val pop_mark: state -> string
  val peek: char t
  val peek_opt: char option t
  val pos: state -> int
  val range: state -> int -> int -> string
  val set_pos: state -> int -> unit
  val advance: int -> unit t
  val char: char -> unit t
  val next: char t
  val (|||): 'a t -> 'a t -> 'a t
  val ws: unit t
  val sp: unit t
  val ws1: unit t
  val (>>>): unit t -> 'a t -> 'a t
  val (<<<): 'a t -> unit t -> 'a t
  val protect: 'a t -> 'a t
  val copy_state: state -> state
  val restore_state: state -> state -> unit
  val peek_before: char -> state -> char
  val peek_after: char -> state -> char
  val pair: 'a t -> 'b t -> ('a * 'b) t
end = struct
  type state =
    {
      mutable str: string;
      mutable pos: int;
      mutable marks: int list;
    }

  let of_string str =
    {str; pos = 0; marks = []}

  let copy_state {str; pos; marks} =
    {str; pos; marks}

  let restore_state st {str; pos; marks} =
    st.str <- str;
    st.pos <- pos;
    st.marks <- marks

  type 'a t =
    state -> 'a

  exception Fail

  let push_mark st =
    st.marks <- st.pos :: st.marks

  let pop_mark st =
    let mark =
      match st.marks with
      | [] -> invalid_arg "pop_mark"
      | mark :: marks -> st.marks <- marks; mark
    in
    String.sub st.str mark (st.pos - mark)

  let char c st =
    if st.pos >= String.length st.str then raise Fail;
    if st.str.[st.pos] <> c then raise Fail;
    st.pos <- st.pos + 1

  let next st =
    if st.pos >= String.length st.str then
      raise Fail
    else
      let c = st.str.[st.pos] in (st.pos <- st.pos + 1; c)

  let peek st =
    if st.pos >= String.length st.str then
      raise Fail
    else
      st.str.[st.pos]

  let peek_opt st =
    if st.pos >= String.length st.str then
      None
    else
      Some st.str.[st.pos]

  let peek_before c st =
    if st.pos = 0 then c
    else st.str.[st.pos - 1]

  let peek_after c st =
    if st.pos + 1 >= String.length st.str then c
    else st.str.[st.pos + 1]

  let pos st =
    st.pos

  let range st pos n =
    String.sub st.str pos n

  let set_pos st pos =
    st.pos <- pos

  let advance n st =
    assert (n >= 0);
    let n = min (String.length st.str - st.pos) n in
    st.pos <- st.pos + n

  let protect p st =
    let st' = copy_state st in
    try p st with e -> restore_state st st'; raise e

  let (|||) p1 p2 st =
    try protect p1 st with Fail -> p2 st

  let ws st =
    let rec loop () =
      match peek st with
      | ' ' | '\t' | '\010'..'\013' -> advance 1 st; loop ()
      | _ -> ()
    in
    try loop () with Fail -> ()

  let sp st =
    let rec loop () =
      match peek st with
      | ' ' | '\t' -> advance 1 st; loop ()
      | _ -> ()
    in
    try loop () with Fail -> ()

  let ws1 st =
    match peek st with
    | ' ' | '\t' | '\010'..'\013' -> advance 1 st; ws st
    | _ -> raise Fail

  let (>>>) p q st =
    p st; q st

  let (<<<) p q st =
    let x = p st in
    q st; x

  let pair p q st =
    let x = p st in
    let y = q st in
    x, y
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

let entity buf st =
  if peek st <> '&' then raise Fail;
  push_mark st;
  advance 1 st;
  match peek st with
  | '#' ->
      advance 1 st;
      begin match peek st with
      | 'x' | 'X' ->
          advance 1 st;
          let rec aux n m =
            if n > 8 then Buffer.add_string buf (pop_mark st)
            else begin
              match peek st with
              | '0'..'9' as c ->
                  advance 1 st;
                  aux (succ n) (m * 16 + Char.code c - Char.code '0')
              | 'a'..'f' as c ->
                  advance 1 st;
                  aux (succ n) (m * 16 + Char.code c - Char.code 'a' + 10)
              | 'A'..'F' as c ->
                  advance 1 st;
                  aux (succ n) (m * 16 + Char.code c - Char.code 'A' + 10)
              | ';' ->
                  advance 1 st;
                  if n = 0 then
                    Buffer.add_string buf (pop_mark st)
                  else
                    let u = if Uchar.is_valid m && m <> 0 then Uchar.of_int m else Uchar.rep in
                    Buffer.add_utf_8_uchar buf u
              | _ ->
                  Buffer.add_string buf (pop_mark st)
              | exception Fail ->
                  Buffer.add_string buf (pop_mark st)
            end
          in
          aux 0 0
      | '0'..'9' ->
          let rec aux n m =
            if n > 8 then Buffer.add_string buf (pop_mark st)
            else begin
              match peek st with
              | '0'..'9' as c ->
                  advance 1 st;
                  aux (succ n) (m * 10 + Char.code c - Char.code '0')
              | ';' ->
                  advance 1 st;
                  if n = 0 then
                    Buffer.add_string buf (pop_mark st)
                  else begin
                    let u = if Uchar.is_valid m && m <> 0 then Uchar.of_int m else Uchar.rep in
                    Buffer.add_utf_8_uchar buf u
                  end
              | _ ->
                  Buffer.add_string buf (pop_mark st)
              | exception Fail ->
                  Buffer.add_string buf (pop_mark st)
            end
          in
          aux 0 0
      | _ ->
          Buffer.add_string buf (pop_mark st)
      | exception Fail ->
          Buffer.add_string buf (pop_mark st)
      end
  | '0'..'9' | 'a'..'z' | 'A'..'9' ->
      push_mark st;
      let rec aux () =
        match peek st with
        | '0'..'9' | 'a'..'z' | 'A'..'Z' ->
            advance 1 st; aux ()
        | ';' ->
            let name = pop_mark st in
            advance 1 st;
            begin match Entities.f name with
            | [] ->
                Buffer.add_string buf (pop_mark st)
            | _ :: _ as cps ->
                List.iter (Buffer.add_utf_8_uchar buf) cps
            end
        | _ ->
            ignore (pop_mark st);
            Buffer.add_string buf (pop_mark st)
        | exception Fail ->
            ignore (pop_mark st);
            Buffer.add_string buf (pop_mark st)
      in
      aux ()
  | _ ->
      Buffer.add_string buf (pop_mark st)
  | exception Fail ->
      Buffer.add_string buf (pop_mark st)

module Pre = struct
  type delim =
    | Ws
    | Punct
    | Other

  type t =
    | Bang_left_bracket
    | Left_bracket of Ast.link_kind
    | Emph of delim * delim * emph_style * int
    | R of inline

  let concat = function
    | [x] -> x
    | l -> Concat l

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

  let classify_delim = function
    | '!' | '"' | '#' | '$' | '%'
    | '&' | '\'' | '(' | ')' | '*' | '+'
    | ',' | '-' | '.' | '/' | ':' | ';'
    | '<' | '=' | '>' | '?' | '@' | '['
    | '\\' | ']' | '^' | '_' | '`' | '{'
    | '|' | '}' | '~' -> Punct
    | ' ' | '\t' | '\010'..'\013' -> Ws
    | _ -> Other

  let to_r : _ -> inline = function
    | Bang_left_bracket -> Text "!["
    | Left_bracket Img -> Text "!["
    | Left_bracket Url -> Text "["
    | Emph (_, _, Star, n) -> Text (String.make n '*')
    | Emph (_, _, Underscore, n) -> Text (String.make n '_')
    | R x -> x

  let rec parse_emph = function
    | Emph (pre, _, q1, n1) as x :: xs when is_opener x ->
        let rec loop acc = function
          | Emph (_, post, q2, n2) as x :: xs when is_closer x && q1 = q2 ->
              let xs =
                if n1 >= 2 && n2 >= 2 then
                  if n2 > 2 then Emph (Punct, post, q2, n2-2) :: xs else xs
                else
                if n2 > 1 then Emph (Punct, post, q2, n2-1) :: xs else xs
              in
              let r =
                let kind = if n1 >= 2 && n2 >= 2 then Strong else Normal in
                R (Emph (kind, q1, concat (List.map to_r (parse_emph (List.rev acc))))) :: xs
              in
              let r =
                if n1 >= 2 && n2 >= 2 then
                  if n1 > 2 then Emph (pre, Punct, q1, n1-2) :: r else r
                else
                if n1 > 1 then Emph (pre, Punct, q1, n1-1) :: r else r
              in
              parse_emph r
          | Emph _ as x :: xs1 as xs when is_opener x ->
              let xs' = parse_emph xs in
              if xs' = xs then loop (x :: acc) xs1 else loop acc xs'
          | x :: xs ->
              loop (x :: acc) xs
          | [] ->
              x :: List.rev acc
        in
        loop [] xs
    | x :: xs ->
        x :: parse_emph xs
    | [] ->
        []

  let parse_emph xs =
    concat (List.map to_r (parse_emph xs))
end

let escape buf st =
  if next st <> '\\' then raise Fail;
  match peek st with
  | c when is_punct c ->
      advance 1 st; Buffer.add_char buf c
  | _ ->
      Buffer.add_char buf '\\'
  | exception Fail ->
      Buffer.add_char buf '\\'

let link_label st =
  if peek st <> '[' then raise Fail;
  advance 1 st;
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | ']' ->
        advance 1 st;
        if Buffer.length buf = 0 then raise Fail;
        Buffer.contents buf
    | '\\' ->
        escape buf st; loop ()
    | '[' ->
        raise Fail
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let normalize s =
  let buf = Buffer.create (String.length s) in
  let rec loop start seen_ws i =
    if i >= String.length s then
      Buffer.contents buf
    else begin
      match s.[i] with
      | ' ' | '\t' | '\010'..'\013' ->
          loop start true (succ i)
      | _ as c ->
          if not start && seen_ws then Buffer.add_char buf ' ';
          Buffer.add_char buf (Char.lowercase_ascii c);
          loop false false (succ i)
    end
  in
  loop true false 0

let tag_name st =
  match peek st with
  | 'a'..'z' | 'A'..'Z' ->
      advance 1 st;
      let rec loop () =
        match peek_opt st with
        | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '-') ->
            advance 1 st; loop ()
        | Some _ | None -> ()
      in
      loop ()
  | _ ->
      raise Fail

let ws_buf buf st =
  let rec loop () =
    match peek_opt st with
    | Some (' ' | '\t' | '\010'..'\013' as c) ->
        Buffer.add_char buf c; advance 1 st; loop ()
    | Some _ | None ->
        ()
  in
  loop ()

let closing_tag st =
  let start = pos st in
  if next st <> '<' then raise Fail;
  if next st <> '/' then raise Fail;
  tag_name st;
  ws st;
  if next st <> '>' then raise Fail;
  range st start (pos st - start)

let list p st =
  let rec loop () =
    match protect p st with
    | () -> loop ()
    | exception Fail -> ()
  in
  loop ()

let single_quoted_attribute st =
  if next st <> '\'' then raise Fail;
  let rec loop () =
    match peek st with
    | '\'' ->
        advance 1 st
    (* | '&' -> *)
    (*     entity buf st; loop () *)
    | _ ->
        advance 1 st; loop ()
  in
  loop ()

let double_quoted_attribute st =
  if next st <> '"' then raise Fail;
  let rec loop () =
    match peek st with
    | '"' ->
        advance 1 st
    (* | '&' -> *)
    (*     entity buf st; loop () *)
    | _ ->
        advance 1 st; loop ()
  in
  loop ()

let unquoted_attribute st =
  let rec loop n =
    match peek st with
    | ' ' | '\t' | '\010'..'\013' | '"' | '\'' | '=' | '<' | '>' | '`' ->
        if n = 0 then raise Fail
    (* | '&' -> *)
    (*     entity buf st; loop () *)
    | _ ->
        advance 1 st; loop (succ n)
  in
  loop 0

let attribute_value st =
  match peek st with
  | '\'' -> single_quoted_attribute st
  | '"' -> double_quoted_attribute st
  | _ -> unquoted_attribute st

let attribute_name st =
  match peek st with
  | 'a'..'z' | 'A'..'Z' | '_' | ':' ->
      advance 1 st;
      let rec loop () =
        match peek_opt st with
        | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | ':' | '-') ->
            advance 1 st; loop ()
        | Some _ | None ->
            ()
      in
      loop ()
  | _ ->
      raise Fail

let option d p st =
  match protect p st with
  | r -> r
  | exception Fail -> d

let some p st =
  Some (p st)

let attribute_value_specification =
  ws >>> char '=' >>> ws >>> attribute_value

let ws1_buf buf st =
  match peek_opt st with
  | Some (' ' | '\t' | '\010'..'\013') ->
      ws_buf buf st
  | Some _ | None ->
      raise Fail

let attribute st =
  ws1 st;
  attribute_name st;
  option () attribute_value_specification st

let open_tag st =
  let start = pos st in
  if next st <> '<' then raise Fail;
  tag_name st;
  list attribute st;
  ws st;
  begin match peek st with
  | '/' -> advance 1 st
  | _ -> ()
  end;
  if next st <> '>' then raise Fail;
  range st start (pos st - start)

let html_comment st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  if next st <> '-' then raise Fail;
  if next st <> '-' then raise Fail;
  Buffer.add_string buf "<!--";
  let rec loop start =
    match peek st with
    | '-' as c ->
        advance 1 st;
        begin match peek st with
        | '-' ->
            advance 1 st;
            if next st <> '>' then raise Fail;
            Buffer.add_string buf "-->";
            Buffer.contents buf
        | '>' when start ->
            raise Fail
        | _ ->
            Buffer.add_char buf c; loop false
        end
    | '>' when start ->
        raise Fail
    | '&' ->
        entity buf st; loop false
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop false
  in
  loop true

let processing_instruction st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '?' then raise Fail;
  Buffer.add_string buf "<?";
  let rec loop () =
    match peek st with
    | '?' as c ->
        advance 1 st;
        begin match peek st with
        | '>' ->
            advance 1 st; Buffer.add_string buf "?>"; Buffer.contents buf
        | _ ->
            Buffer.add_char buf c; loop ()
        end
    | '&' ->
        entity buf st; loop ()
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let cdata_section st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  if next st <> '[' then raise Fail;
  if next st <> 'C' then raise Fail;
  if next st <> 'D' then raise Fail;
  if next st <> 'A' then raise Fail;
  if next st <> 'T' then raise Fail;
  if next st <> 'A' then raise Fail;
  if next st <> '[' then raise Fail;
  Buffer.add_string buf "<![CDATA[";
  let rec loop () =
    match peek st with
    | ']' as c ->
        advance 1 st;
        begin match peek st with
        | ']' as c1 ->
            advance 1 st;
            begin match peek st with
            | '>' ->
                advance 1 st; Buffer.add_string buf "]]>"; Buffer.contents buf
            | _ ->
                Buffer.add_char buf c; Buffer.add_char buf c1; loop ()
            end
        | _ ->
            Buffer.add_char buf c; loop ()
        end
    | '&' ->
        entity buf st; loop ()
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let declaration st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  Buffer.add_string buf "<!";
  match peek st with
  | 'A'..'Z' ->
      let rec loop () =
        match peek st with
        | 'A'..'Z' as c ->
            advance 1 st; Buffer.add_char buf c; loop ()
        | ' ' | '\t' | '\010'..'\013' ->
            ws1_buf buf st;
            let rec loop () =
              match peek st with
              | '>' as c ->
                  advance 1 st; Buffer.add_char buf c; Buffer.contents buf
              | '&' ->
                  entity buf st; loop ()
              | _ as c ->
                  advance 1 st; Buffer.add_char buf c; loop ()
            in
            loop ()
        | _ ->
            raise Fail
      in
      loop ()
  | _ ->
      raise Fail


let link_destination st =
  let buf = Buffer.create 17 in
  match peek st with
  | '<' ->
      advance 1 st;
      let rec loop () =
        match peek st with
        | '>' ->
            advance 1 st; Buffer.contents buf
        | ' ' | '\t' | '\010'..'\013' | '<' ->
            raise Fail
        | '\\' ->
            escape buf st; loop ()
        | '&' ->
            entity buf st; loop ()
        | _ as c ->
            advance 1 st; Buffer.add_char buf c; loop ()
      in
      loop ()
  | _ ->
      let rec loop n =
        match peek st with
        | '(' as c ->
            advance 1 st; Buffer.add_char buf c; loop (succ n)
        | ')' as c ->
            if n = 0 then begin
              if Buffer.length buf = 0 then raise Fail;
              Buffer.contents buf
            end else begin
              advance 1 st;
              Buffer.add_char buf c;
              loop (pred n)
            end
        | '\\' ->
            escape buf st; loop n
        | '&' ->
            entity buf st; loop n
        | ' ' | '\t' | '\x00'..'\x1F' | '\x7F' | '\x80'..'\x9F' ->
            if n > 0 || Buffer.length buf = 0 then raise Fail;
            Buffer.contents buf
        | exception Fail ->
            if n > 0 || Buffer.length buf = 0 then raise Fail;
            Buffer.contents buf
        | _ as c ->
            advance 1 st; Buffer.add_char buf c; loop n
      in
      loop 0

let eol st =
  match peek st with
  | '\n' -> advance 1 st
  | _ -> raise Fail
  | exception Fail -> ()

let link_title st =
  let buf = Buffer.create 17 in
  match peek st with
  | '\'' ->
      advance 1 st;
      let rec loop () =
        match peek st with
        | '\\' ->
            escape buf st; loop ()
        | '&' ->
            entity buf st; loop ()
        | '\'' ->
            advance 1 st; Buffer.contents buf
        | _ as c ->
            advance 1 st; Buffer.add_char buf c; loop ()
      in
      loop ()
  | '"' ->
      advance 1 st;
      let rec loop () =
        match peek st with
        | '\\' ->
            escape buf st; loop ()
        | '&' ->
            entity buf st; loop ()
        | '"' ->
            advance 1 st; Buffer.contents buf
        | _ as c ->
            advance 1 st; Buffer.add_char buf c; loop ()
      in
      loop ()
  | '(' ->
      advance 1 st;
      let rec loop () =
        match peek st with
        | '\\' ->
            escape buf st; loop ()
        | '&' ->
            entity buf st; loop ()
        | ')' ->
            advance 1 st;
            Buffer.contents buf
        | _ as c ->
            advance 1 st; Buffer.add_char buf c; loop ()
      in
      loop ()
  | _ ->
      raise Fail

let space st =
  match peek st with
  | ' ' -> advance 1 st
  | _ -> raise Fail

let many p st =
  try
    while true do p st done
  with Fail ->
    ()

let scheme st =
  match peek st with
  | 'a'..'z' | 'A'..'Z' ->
      let rec loop n =
        if n < 32 then begin
          match peek_opt st with
          | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '+' | '.' | '-') ->
              advance 1 st; loop (succ n)
          | Some _ | None ->
              n
        end else n
      in
      let n = loop 0 in
      if n < 2 then raise Fail
  | _ ->
      raise Fail

let absolute_uri st =
  let p = pos st in
  scheme st;
  if next st <> ':' then raise Fail;
  let rec loop () =
    match peek_opt st with
    | Some (' ' | '\t' | '\010'..'\013' | '\x00'..'\x1F' | '\x7F'..'\x9F' | '<' | '>') | None ->
        let txt = range st p (pos st - p) in
        txt, txt
    | Some _ ->
        advance 1 st; loop ()
  in
  loop ()

let email_address st =
  let p = pos st in
  let rec loop n =
    match peek st with
    | 'a'..'z' | 'A'..'Z' | '0'..'9'
    | '.' | '!' | '#' | '$' | '%' | '&' | '\'' | '*'
    | '+' | '/' | '=' | '?' | '^' | '_' | '`' | '{' | '|'
    | '}' | '~' | '-' ->
        advance 1 st; loop (succ n)
    | '@' ->
        advance 1 st;
        let label st =
          let let_dig st =
            match peek st with
            | 'a'..'z' | 'A'..'Z' | '0'..'9' -> advance 1 st; false
            | '-' -> advance 1 st; true
            | _ -> raise Fail
          in
          if let_dig st then raise Fail;
          let rec loop last =
            match let_dig st with
            | r ->
                loop r
            | exception Fail ->
                if last then raise Fail
          in
          loop false
        in
        label st;
        list (char '.' >>> label) st;
        let txt = range st p (pos st - p) in
        txt, "mailto:" ^ txt
    | _ ->
        raise Fail
  in
  loop 0

let autolink st =
  match peek st with
  | '<' ->
      advance 1 st;
      let label, destination = (absolute_uri ||| email_address) st in
      if next st <> '>' then raise Fail;
      {Ast.label; destination; title = None}
  | _ ->
      raise Fail

let rec inline defs st =
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
  let rec reference_link kind acc st =
    let st0 = copy_state st in
    match protect link_label st with
    | lab ->
        let reflink lab' =
          let s = normalize lab' in
          let lab = inline [] (of_string lab) in
          match List.find_opt (fun {Ast.label; _} -> label = s) defs with
          | Some def ->
              loop (Pre.R (Ref (kind, lab, def)) :: text acc) st
          | None ->
              Buffer.add_char buf '[';
              let acc = Pre.R lab :: text acc in
              Buffer.add_char buf ']';
              loop acc st
        in
        begin match peek_opt st with
        | Some '[' ->
            if peek_after '\000' st = ']' then
              (advance 2 st; reflink lab)
            else begin
              match protect link_label st with
              | lab -> reflink lab
              | exception Fail -> reflink lab
            end
        | Some '(' ->
            restore_state st st0; (* hack *)
            advance 1 st;
            loop (Left_bracket kind :: text acc) st
        | Some _ | None ->
            reflink lab
        end
    | exception Fail ->
        advance 1 st; loop (Left_bracket kind :: text acc) st
  and loop acc st =
    match peek st with
    | '<' as c ->
        begin match protect autolink st with
        | def ->
            loop (Pre.R (Link (Url, {def with label = Text def.label})) :: text acc) st
        | exception Fail ->
          begin match
            protect (closing_tag ||| open_tag ||| html_comment |||
                     declaration ||| cdata_section ||| processing_instruction) st
          with
          | tag ->
              loop (Pre.R (Html tag) :: text acc) st
          | exception Fail ->
              advance 1 st; Buffer.add_char buf c; loop acc st
          end
        end
    | '\n' ->
        advance 1 st; sp st; loop (Pre.R Soft_break :: text acc) st
    | ' ' as c ->
        advance 1 st;
        begin match peek_opt st with
        | Some ' ' ->
            begin match protect (many space >>> char '\n' >>> many space) st with
            | () ->
                loop (Pre.R Hard_break :: text acc) st
            | exception Fail ->
                advance 1 st;
                Buffer.add_string buf "  "; loop acc st
            end
        | Some '\n' ->
            loop acc st
        | Some _ | None ->
            Buffer.add_char buf c; loop acc st
        end
    | '`' ->
        let pos = pos st in
        let rec loop2 n =
          match peek_opt st with
          | Some '`' ->
              advance 1 st; loop2 (succ n)
          | Some _ ->
              let acc = text acc in
              let bufcode = Buffer.create 17 in
              let rec loop3 start seen_ws m =
                match peek_opt st with
                | Some '`' ->
                    advance 1 st; loop3 start seen_ws (succ m)
                | Some (' ' | '\t' | '\010'..'\013') ->
                    if m = n then
                      loop (Pre.R (Code (Buffer.contents bufcode)) :: acc) st
                    else begin
                      if m > 0 then begin
                        if not start && seen_ws then Buffer.add_char bufcode ' ';
                        Buffer.add_string bufcode (String.make m '`');
                      end;
                      advance 1 st; loop3 (start && m = 0) true 0
                    end
                | Some c ->
                    if m = n then
                      loop (Pre.R (Code (Buffer.contents bufcode)) :: acc) st
                    else begin
                      advance 1 st;
                      if not start && seen_ws then Buffer.add_char bufcode ' ';
                      if m > 0 then Buffer.add_string bufcode (String.make m '`');
                      Buffer.add_char bufcode c;
                      loop3 false false 0
                    end
                | None ->
                    if m = n then
                      loop (Pre.R (Code (Buffer.contents bufcode)) :: acc) st
                    else begin
                      Buffer.add_string buf (range st pos n);
                      set_pos st (pos + n);
                      loop acc st
                    end
              in
              loop3 true false 0
          | None ->
              Buffer.add_string buf (String.make n '`'); loop acc st
        in
        loop2 0
    | '\\' as c ->
        advance 1 st;
        begin match peek_opt st with
        | Some '\n' ->
            advance 1 st; loop (Pre.R Hard_break :: text acc) st
        | Some c when is_punct c ->
            advance 1 st; Buffer.add_char buf c; loop acc st
        | Some _ | None ->
            Buffer.add_char buf c; loop acc st
        end
    | '!' as c ->
        advance 1 st;
        begin match peek_opt st with
        | Some '[' ->
            reference_link Img (text acc) st
        | Some _ | None ->
            Buffer.add_char buf c; loop acc st
        end
    | '&' ->
        entity buf st; loop acc st
    | ']' ->
        advance 1 st;
        let acc = text acc in
        let rec aux seen_link xs = function
          | Pre.Left_bracket Url :: _ when seen_link ->
              Buffer.add_char buf ']'; loop acc st
          | Left_bracket k :: acc' ->
              begin match peek_opt st with
              | Some '(' ->
                  begin match
                    protect
                      (char '(' >>>
                       option ("", None)
                         (pair link_destination (option None (ws1 >>> some link_title)))
                       <<< ws <<< char ')') st
                  with
                  | destination, title ->
                      let r =
                        let label = Pre.parse_emph xs in
                        Link (k, {Ast.label; destination; title})
                      in
                      loop (Pre.R r :: acc') st
                  | exception Fail ->
                      Buffer.add_char buf ']'; loop acc st
                  end
              | Some '[' ->
                  assert false
              | Some _ | None ->
                  Buffer.add_char buf ']'; loop acc st
              end
          | Pre.R (Link (Url, _) | Ref (Url, _, _)) as x :: acc' ->
              aux true (x :: xs) acc'
          | x :: acc' ->
              aux seen_link (x :: xs) acc'
          | [] ->
              Buffer.add_char buf ']'; loop acc st
        in
        aux false [] acc
    | '[' ->
        reference_link Url acc st
    | '*' | '_' as c ->
        let pre = peek_before ' ' st in
        let f post n st =
          let pre = pre |> Pre.classify_delim in
          let post = post |> Pre.classify_delim in
          let e = if c = '*' then Ast.Star else Underscore in
          loop (Pre.Emph (pre, post, e, n) :: text acc) st
        in
        let rec aux n =
          match peek_opt st with
          | Some c1 when c1 = c -> advance 1 st; aux (succ n)
          | Some c1 -> f c1 n st
          | None -> f ' ' n st
        in
        aux 0
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop acc st
    | exception Fail ->
        Pre.parse_emph (List.rev (text acc))
  in
  loop [] st

let sp3 st =
  match peek st with
  | ' ' ->
      advance 1 st;
      begin match peek st with
      | ' ' ->
          advance 1 st;
          begin match peek st with
          | ' ' -> advance 1 st; 3
          | _ -> 2
          | exception Fail -> 2
          end
      | _ -> 1
      | exception Fail -> 1
      end
  | _ -> 0
  | exception Fail -> 0

let link_reference_definition st =
  let ws st =
    let rec loop seen_nl =
      match peek st with
      | ' ' | '\t' | '\011'..'\013' -> advance 1 st; loop seen_nl
      | '\n' when not seen_nl -> advance 1 st; loop true
      | _ -> ()
      | exception Fail -> ()
    in
    loop false
  in
  let ws1 st =
    match next st with
    | ' ' | '\t' | '\010'..'\013' -> ws st
    | _ -> raise Fail
  in
  ignore (sp3 st);
  let label = link_label st in
  if next st <> ':' then raise Fail;
  ws st;
  let destination = link_destination st in
  match protect (ws1 >>> link_title <<< sp <<< eol) st with
  | title ->
      {Ast.label; destination; title = Some title}
  | exception Fail ->
      (sp >>> eol) st;
      {Ast.label; destination; title = None}

let link_reference_definitions st =
  let rec loop acc =
    match protect link_reference_definition st with
    | def ->
        loop (def :: acc)
    | exception Fail ->
        acc, pos st
  in
  loop []

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

  let pos st =
    st.pos

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
                  let u = if Uchar.is_valid m && m <> 0 then Uchar.of_int m else Uchar.rep in
                  Buffer.add_utf_8_uchar buf u
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

module Pre = Inline.Pre

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
          Buffer.add_char buf c;
          loop false false (succ i)
    end
  in
  loop true false 0

let tag_name st =
  match peek st with
  | 'a'..'z' | 'A'..'Z' ->
      push_mark st;
      advance 1 st;
      let rec loop () =
        match peek st with
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' ->
            advance 1 st; loop ()
        | _ ->
            pop_mark st
        | exception Fail ->
            pop_mark st
      in
      loop ()
  | _ ->
      raise Fail

let closing_tag st =
  if next st <> '<' then raise Fail;
  if next st <> '/' then raise Fail;
  let name = tag_name st in
  ws st;
  if next st <> '>' then raise Fail;
  name

let list p st =
  let rec loop acc =
    match protect p st with
    | r -> loop (r :: acc)
    | exception Fail -> List.rev acc
  in
  loop []

let single_quoted_attribute st =
  if next st <> '\'' then raise Fail;
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | '\'' ->
        advance 1 st; Buffer.contents buf
    | '&' ->
        entity buf st; loop ()
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let double_quoted_attribute st =
  if next st <> '"' then raise Fail;
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | '"' ->
        advance 1 st; Buffer.contents buf
    | '&' ->
        entity buf st; loop ()
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let unquoted_attribute st =
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | ' ' | '\t' | '\010'..'\013' | '"' | '\'' | '=' | '<' | '>' | '`' ->
        if Buffer.length buf = 0 then raise Fail;
        Buffer.contents buf
    | '&' ->
        entity buf st; loop ()
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let attribute_value st =
  match peek st with
  | '\'' -> single_quoted_attribute st
  | '"' -> double_quoted_attribute st
  | _ -> unquoted_attribute st

let attribute_name st =
  match peek st with
  | 'a'..'z' | 'A'..'Z' | '_' | ':' ->
      push_mark st;
      advance 1 st;
      let rec loop () =
        match peek st with
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | ':' | '-' ->
            advance 1 st; loop ()
        | _ ->
            pop_mark st
        | exception Fail ->
            pop_mark st
      in
      loop ()
  | _ ->
      raise Fail

let option p st =
  match protect p st with
  | r -> Some r
  | exception Fail -> None

let attribute_value_specification =
  ws >>> char '=' >>> ws >>> attribute_value

let attribute st =
  ws1 st;
  let name = attribute_name st in
  let value = option attribute_value_specification st in
  name, value

let open_tag st =
  if next st <> '<' then raise Fail;
  let name = tag_name st in
  let attributes = list attribute st in
  ws st;
  begin match peek st with
  | '/' -> advance 1 st
  | _ -> ()
  end;
  if next st <> '>' then raise Fail;
  (name, attributes)

let html_comment st =
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  if next st <> '-' then raise Fail;
  if next st <> '-' then raise Fail;
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | '-' as c ->
        advance 1 st;
        begin match peek st with
        | '-' ->
            advance 1 st;
            if next st <> '>' then raise Fail;
            Buffer.contents buf
        | _ ->
            advance 1 st; Buffer.add_char buf c; loop ()
        end
    | '&' ->
        entity buf st; loop ()
    | _ as c ->
        advance 1 st; Buffer.add_char buf c; loop ()
  in
  loop ()

let processing_instruction st =
  if next st <> '<' then raise Fail;
  if next st <> '?' then raise Fail;
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | '?' as c ->
        advance 1 st;
        begin match peek st with
        | '>' ->
            advance 1 st; Buffer.contents buf
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
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  if next st <> '[' then raise Fail;
  if next st <> 'C' then raise Fail;
  if next st <> 'D' then raise Fail;
  if next st <> 'A' then raise Fail;
  if next st <> 'T' then raise Fail;
  if next st <> 'A' then raise Fail;
  if next st <> '[' then raise Fail;
  let buf = Buffer.create 17 in
  let rec loop () =
    match peek st with
    | ']' as c ->
        advance 1 st;
        begin match peek st with
        | ']' as c1 ->
            advance 1 st;
            begin match peek st with
            | '>' ->
                advance 1 st; Buffer.contents buf
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
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  match peek st with
  | 'A'..'Z' ->
      push_mark st;
      let rec loop () =
        match peek st with
        | 'A'..'Z' ->
            advance 1 st; loop ()
        | ' ' | '\t' | '\010'..'\013' ->
            let name = pop_mark st in
            ws1 st;
            let buf = Buffer.create 17 in
            let rec loop () =
              match peek st with
              | '>' ->
                  advance 1 st;
                  name, Buffer.contents buf
              | '&' ->
                  entity buf st; loop ()
              | _ as c ->
                  advance 1 st; Buffer.add_char buf c; loop ()
              | exception e ->
                  ignore (pop_mark st);
                  raise e
            in
            loop ()
        | _ ->
            ignore (pop_mark st);
            raise Fail
      in
      loop ()
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
  let rec loop acc st =
    match peek st with
    | '<' as c ->
        begin match protect closing_tag st with
        | tag ->
            loop (Pre.R (Html (Printf.sprintf "</%s>" tag)) :: text acc) st
        | exception Fail ->
            begin match protect open_tag st with
            | tag, _attributes ->
                loop (Pre.R (Html (Printf.sprintf "<%s ...>" tag)) :: text acc) st
            | exception Fail ->
                begin match protect html_comment st with
                | comment ->
                    loop (Pre.R (Html (Printf.sprintf "<!--%s-->" comment)) :: text acc) st
                | exception Fail ->
                    begin match protect declaration st with
                    | x, y ->
                        loop (Pre.R (Html (Printf.sprintf "<!%s %s>" x y)) :: text acc) st
                    | exception Fail ->
                        begin match protect cdata_section st with
                        | x ->
                            loop (Pre.R (Html (Printf.sprintf "<![CDATA[%s]]>" x)) :: text acc) st
                        | exception Fail ->
                            begin match protect processing_instruction st with
                            | x ->
                                loop (Pre.R (Html (Printf.sprintf "<?%s?>" x)) :: text acc) st
                            | exception Fail ->
                                advance 1 st; Buffer.add_char buf c; loop acc st
                            end
                        end
                    end
                end
            end
        end
    | ' ' as c ->
        advance 1 st;
        begin match protect (sp >>> char '\n' >>> sp) st with
        | () ->
            loop (Pre.R Hard_break :: text acc) st
        | exception Fail ->
            Buffer.add_char buf c; loop acc st
        end
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
            advance 1 st; loop (Bang_left_bracket :: text acc) st
        | Some _ | None ->
            Buffer.add_char buf c; loop acc st
        end
    | '&' ->
        entity buf st; loop acc st
    | '[' ->
        begin match protect link_label st with
        | lab1 ->
            begin match peek_opt st with
            | Some '[' ->
                assert false
            | Some _ | None ->
                let s = normalize lab1 in
                begin match List.find_opt (fun {Ast.label; _} -> label = s) defs with
                | Some def ->
                    let txt = inline [] (of_string lab1) in
                    loop (Pre.R (Url_ref (txt, def)) :: text acc) st
                | None ->
                    assert false
                end
            end
        | exception Fail ->
            advance 1 st; loop (Left_bracket :: text acc) st
        end
    (* | '*' | '_' as c -> *)
    (*     let pre = peek_before ' ' st |> Pre.classify_delim in *)
    (*     let f post n st = *)
    (*       let post = post |> Pre.classify_delim in *)
    (*       let e = if c = '*' then Ast.Star else Underscore in *)
    (*       loop (Pre.Emph (pre, post, e, n) :: text acc) st *)
    (*     in *)
    (*     let rec aux n = *)
    (*       match peek st with *)
    (*       | c1 when c1 = c -> advance 1 st; aux (succ n) *)
    (*       | c1 -> f c1 n st *)
    (*       | exception End_of_file -> f ' ' n st *)
    (*     in *)
    (*     aux 0 *)
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
            Buffer.add_char buf c; loop (succ n)
        | ')' as c ->
            if n = 0 then begin
              if Buffer.length buf = 0 then raise Fail;
              Buffer.contents buf
            end else begin
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
  let title = (option (ws1 >>> link_title) <<< sp <<< eol) st in
  {Ast.label; destination; title}

let link_reference_definitions st =
  let rec loop acc =
    match protect link_reference_definition st with
    | def ->
        loop (def :: acc)
    | exception Fail ->
        acc, pos st
  in
  loop []

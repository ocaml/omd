open Ast
open Compat
module Sub = StrSlice

exception Fail
(** Raised when a parser fails, used for control flow rather than error
    handling *)

(** Stateful parser combinators *)
module P : sig
  type state
  type 'a t = state -> 'a

  val of_string : string -> state

  val peek : char option t
  (** [Some c] if [c] is the next character in the input, or [None]
      if the input is exhausted.

      NOTE: Does not advance the state. *)

  val peek_exn : char t
  (** the next character in the input, or raises [Fail] if the
      input is exhausted.

      NOTE: Does not advance the state. *)

  val peek_before : char -> state -> char
  (** the previous character in the input, or the next
      character, if we are at the start of the input.

      NOTE: Does not advance the state. *)

  val peek_after : char -> state -> char
  (** the character after the next in the input, or the next
      character, if we are at the end of the input.

      NOTE: Does not advance the state. *)

  val pos : state -> int
  val range : state -> int -> int -> string
  val set_pos : state -> int -> unit

  val junk : unit t
  (** ignores the next character in the input *)

  val char : char -> unit t
  (** accepts a [c] *)

  val next : char t

  val ws : unit t
  (** accepts 0 or more white space characters *)

  val sp : unit t
  (** accepts 0 or more spaces or tabs *)

  val ws1 : unit t
  (** accepts 1 or more spaces or tabs, fails if none is found *)

  val ( ||| ) : 'a t -> 'a t -> 'a t
  (** [p ||| q] tries to accept [p], but in case [p] fails, it accepts [q]
      (which can fail). *)

  val ( >>> ) : unit t -> 'a t -> 'a t
  (** [p >>> q] accepts [p] followed by [q], returning whatever [q] does *)

  val ( <<< ) : 'a t -> unit t -> 'a t
  (** [p >>> q] accepts [p] followed by [q], returning whatever [q] does *)

  val protect : 'a t -> 'a t
  (** run the given parser, resetting the state back to it's initial condition
      if the parser fails *)

  val pair : 'a t -> 'b t -> ('a * 'b) t

  val on_sub : (Sub.t -> 'a * Sub.t) -> 'a t
  (** Given a function [f] that takes a prefix of a string slice to a value [x]
      of type ['a] and some remainder of the slice, [on_sub f] produces [x] from
      the state, and advances the input the length of the slice that was
      consumed by [f]. *)
end = struct
  type state =
    { str : string
    ; mutable pos : int
    }

  let of_string str = { str; pos = 0 }

  type 'a t = state -> 'a

  let ensure_chars_remain st = if st.pos >= String.length st.str then raise Fail

  let char c st =
    ensure_chars_remain st;
    if st.str.[st.pos] <> c then raise Fail;
    st.pos <- st.pos + 1

  let next st =
    ensure_chars_remain st;
    let c = st.str.[st.pos] in
    st.pos <- st.pos + 1;
    c

  let peek st =
    if st.pos >= String.length st.str then None else Some st.str.[st.pos]

  let peek_exn st = match peek st with Some c -> c | None -> raise Fail
  let peek_before c st = if st.pos = 0 then c else st.str.[st.pos - 1]

  let peek_after c st =
    if st.pos + 1 >= String.length st.str then c else st.str.[st.pos + 1]

  let pos st = st.pos
  let range st pos n = String.sub st.str pos n
  let set_pos st pos = st.pos <- pos
  let junk st = if st.pos < String.length st.str then st.pos <- st.pos + 1

  let protect p st =
    let off = pos st in
    try p st
    with e ->
      set_pos st off;
      raise e

  let ( ||| ) p1 p2 st = try protect p1 st with Fail -> p2 st

  let ws st =
    let rec loop () =
      match peek_exn st with
      | ' ' | '\t' | '\010' .. '\013' ->
          junk st;
          loop ()
      | _ -> ()
    in
    try loop () with Fail -> ()

  let sp st =
    let rec loop () =
      match peek_exn st with
      | ' ' | '\t' ->
          junk st;
          loop ()
      | _ -> ()
    in
    try loop () with Fail -> ()

  let ws1 st =
    match peek_exn st with
    | ' ' | '\t' | '\010' .. '\013' ->
        junk st;
        ws st
    | _ -> raise Fail

  let ( >>> ) p q st =
    p st;
    q st

  let ( <<< ) p q st =
    let x = p st in
    q st;
    x

  let pair p q st =
    let x = p st in
    let y = q st in
    (x, y)

  let on_sub fn st =
    let result, s = fn (Sub.of_string ~off:st.pos st.str) in
    st.pos <- Sub.get_offset s;
    result
end

type html_kind =
  | Hcontains of string list
  | Hblank

type code_block_kind =
  | Tilde
  | Backtick

type t =
  | Lempty
  | Lblockquote of Sub.t
  | Lthematic_break
  | Latx_heading of int * string * attributes
  | Lsetext_heading of int * int
  | Lfenced_code of int * int * code_block_kind * (string * string) * attributes
  | Lindented_code of Sub.t
  | Lhtml of bool * html_kind
  | Llist_item of list_type * int * Sub.t
  | Lparagraph
  | Ldef_list of string

let sp3 s =
  match Sub.head s with
  | Some ' ' -> (
      let s = Sub.tail s in
      match Sub.head s with
      | Some ' ' -> (
          let s = Sub.tail s in
          match Sub.head s with
          | Some ' ' -> (3, Sub.tail s)
          | Some _ | None -> (2, s))
      | Some _ | None -> (1, s))
  | Some _ | None -> (0, s)

let ( ||| ) p1 p2 s = try p1 s with Fail -> p2 s

let rec trim_ws ?(rev = false) s =
  let inspect, drop =
    if rev then (Sub.last, Sub.drop_last) else (Sub.head, Sub.tail)
  in
  match inspect s with
  | Some (' ' | '\t' | '\010' .. '\013') -> trim_ws ~rev (drop s)
  | None | Some _ -> s

let is_empty s = Sub.is_empty (trim_ws s)

let thematic_break s =
  match Sub.head s with
  | Some (('*' | '_' | '-') as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 -> loop (succ n) (Sub.tail s)
        | Some (' ' | '\t' | '\010' .. '\013') -> loop n (Sub.tail s)
        | Some _ -> raise Fail
        | None ->
            if n < 3 then raise Fail;
            Lthematic_break
      in
      loop 1 (Sub.tail s)
  | Some _ | None -> raise Fail

let setext_heading s =
  match Sub.head s with
  | Some (('-' | '=') as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 -> loop (succ n) (Sub.tail s)
        | Some _ | None ->
            if not (Sub.is_empty (trim_ws s)) then raise Fail;
            if c = '-' && n = 1 then raise Fail;
            (* can be interpreted as an empty list item *)
            Lsetext_heading ((if c = '-' then 2 else 1), n)
      in
      loop 1 (Sub.tail s)
  | Some _ | None -> raise Fail

let is_punct = function
  | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-'
  | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^'
  | '_' | '`' | '{' | '|' | '}' | '~' ->
      true
  | _ -> false

let parse_attributes = function
  | None -> []
  | Some s -> (
      let attributes = String.split_on_char ' ' s in
      let f (id, classes, acc) s =
        if s = "" then (id, classes, acc)
        else
          match s.[0] with
          | '#' -> (Some (String.sub s 1 (String.length s - 1)), classes, acc)
          | '.' -> (id, String.sub s 1 (String.length s - 1) :: classes, acc)
          | _ -> (
              let attr = String.split_on_char '=' s in
              match attr with
              | [] -> (id, classes, acc)
              | h :: t -> (id, classes, (h, String.concat "=" t) :: acc))
      in
      let id, classes, acc = List.fold_left f (None, [], []) attributes in
      let acc = List.rev acc in
      let acc =
        if classes <> [] then
          ("class", String.concat " " (List.rev classes)) :: acc
        else acc
      in
      match id with Some id -> ("id", id) :: acc | None -> acc)

let attribute_string s =
  let buf = Buffer.create 64 in
  let rec loop s =
    match Sub.head s with
    | None -> (Sub.of_string (Buffer.contents buf), None)
    | Some ('\\' as c) -> (
        let s = Sub.tail s in
        match Sub.head s with
        | Some c when is_punct c ->
            Buffer.add_char buf c;
            loop (Sub.tail s)
        | Some _ | None ->
            Buffer.add_char buf c;
            loop s)
    | Some '{' ->
        let buf' = Buffer.create 64 in
        let rec loop' s =
          match Sub.head s with
          | Some '}' -> (
              let s = Sub.tail s in
              match Sub.head s with
              | Some _ ->
                  Buffer.add_char buf '{';
                  Buffer.add_buffer buf buf';
                  Buffer.add_char buf '}';
                  loop s
              | None ->
                  ( Sub.of_string (Buffer.contents buf)
                  , Some (Buffer.contents buf') ))
          | None ->
              Buffer.add_char buf '{';
              Buffer.add_buffer buf buf';
              (Sub.of_string (Buffer.contents buf), None)
          | Some '{' ->
              Buffer.add_char buf '{';
              Buffer.add_buffer buf buf';
              Buffer.reset buf';
              loop' (Sub.tail s)
          | Some c ->
              Buffer.add_char buf' c;
              loop' (Sub.tail s)
        in
        loop' (Sub.tail s)
    | Some c ->
        Buffer.add_char buf c;
        loop (Sub.tail s)
  in
  let s', a = loop (trim_ws s) in
  (s', parse_attributes a)

let atx_heading s =
  let rec loop n s =
    if n > 6 then raise Fail;
    match Sub.head s with
    | Some '#' -> loop (succ n) (Sub.tail s)
    | Some (' ' | '\t' | '\010' .. '\013') ->
        let s, a =
          match Sub.last s with Some '}' -> attribute_string s | _ -> (s, [])
        in
        let s = trim_ws ~rev:true (trim_ws s) in
        let rec loop t =
          match Sub.last t with
          | Some '#' -> loop (Sub.drop_last t)
          | Some (' ' | '\t' | '\010' .. '\013') | None -> trim_ws ~rev:true t
          | Some _ -> s
        in
        Latx_heading (n, Sub.to_string (trim_ws (loop s)), a)
    | Some _ -> raise Fail
    | None -> Latx_heading (n, Sub.to_string s, [])
  in
  loop 0 s

let entity s =
  match Sub.take 2 s with
  | '#' :: ('x' | 'X') :: _ ->
      let rec loop m n s =
        if m > 6 then raise Fail;
        match Sub.head s with
        | Some ('a' .. 'f' as c) ->
            loop
              (succ m)
              ((n * 16) + Char.code c - Char.code 'a' + 10)
              (Sub.tail s)
        | Some ('A' .. 'F' as c) ->
            loop
              (succ m)
              ((n * 16) + Char.code c - Char.code 'A' + 10)
              (Sub.tail s)
        | Some ('0' .. '9' as c) ->
            loop (succ m) ((n * 16) + Char.code c - Char.code '0') (Sub.tail s)
        | Some ';' ->
            if m = 0 then raise Fail;
            let u =
              if n = 0 || not (Uchar.is_valid n) then Uchar.rep
              else Uchar.of_int n
            in
            ([ u ], Sub.tail s)
        | Some _ | None -> raise Fail
      in
      loop 0 0 (Sub.drop 2 s)
  | '#' :: _ ->
      let rec loop m n s =
        if m > 7 then raise Fail;
        match Sub.head s with
        | Some ('0' .. '9' as c) ->
            loop (succ m) ((n * 10) + Char.code c - Char.code '0') (Sub.tail s)
        | Some ';' ->
            if m = 0 then raise Fail;
            let u =
              if n = 0 || not (Uchar.is_valid n) then Uchar.rep
              else Uchar.of_int n
            in
            ([ u ], Sub.tail s)
        | Some _ | None -> raise Fail
      in
      loop 0 0 (Sub.tail s)
  | ('a' .. 'z' | 'A' .. 'Z') :: _ ->
      let rec loop len t =
        match Sub.head t with
        | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') ->
            loop (succ len) (Sub.tail t)
        | Some ';' -> (
            let name = Sub.to_string (Sub.sub ~len s) in
            match Entities.f name with
            | [] -> raise Fail
            | cps -> (cps, Sub.tail t))
        | Some _ | None -> raise Fail
      in
      loop 1 (Sub.tail s)
  | _ -> raise Fail

let info_string c s =
  let buf = Buffer.create 17 in
  let s, a =
    match Sub.last s with Some '}' -> attribute_string s | _ -> (s, [])
  in
  let s = trim_ws ~rev:true (trim_ws s) in
  let rec loop s =
    match Sub.head s with
    | Some (' ' | '\t' | '\010' .. '\013') | None ->
        if c = '`' && Sub.exists (function '`' -> true | _ -> false) s then
          raise Fail;
        ((Buffer.contents buf, Sub.to_string (trim_ws s)), a)
    | Some '`' when c = '`' -> raise Fail
    | Some ('\\' as c) -> (
        let s = Sub.tail s in
        match Sub.head s with
        | Some c when is_punct c ->
            Buffer.add_char buf c;
            loop (Sub.tail s)
        | Some _ | None ->
            Buffer.add_char buf c;
            loop s)
    | Some ('&' as c) -> (
        let s = Sub.tail s in
        match entity s with
        | ul, s ->
            List.iter (Buffer.add_utf_8_uchar buf) ul;
            loop s
        | exception Fail ->
            Buffer.add_char buf c;
            loop s)
    | Some c ->
        Buffer.add_char buf c;
        loop (Sub.tail s)
  in
  loop (trim_ws s)

let fenced_code ind s =
  match Sub.head s with
  | Some (('`' | '~') as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 -> loop (succ n) (Sub.tail s)
        | Some _ | None ->
            if n < 3 then raise Fail;
            let s, a = info_string c s in
            let c = if c = '`' then Backtick else Tilde in
            Lfenced_code (ind, n, c, s, a)
      in
      loop 1 (Sub.tail s)
  | Some _ | None -> raise Fail

let indent s =
  let rec loop n s =
    match Sub.head s with
    | Some ' ' -> loop (n + 1) (Sub.tail s)
    | Some '\t' -> loop (n + 4) (Sub.tail s)
    | Some _ | None -> n
  in
  loop 0 s

let unordered_list_item ind s =
  match Sub.head s with
  | Some (('+' | '-' | '*') as c) ->
      let s = Sub.tail s in
      if is_empty s then Llist_item (Bullet c, 2 + ind, s)
      else
        let n = indent s in
        if n = 0 then raise Fail;
        let n = if n <= 4 then n else 1 in
        Llist_item (Bullet c, n + 1 + ind, Sub.offset n s)
  | Some _ | None -> raise Fail

let ordered_list_item ind s =
  let rec loop n m s =
    match Sub.head s with
    | Some ('0' .. '9' as c) ->
        if n >= 9 then raise Fail;
        loop (succ n) ((m * 10) + Char.code c - Char.code '0') (Sub.tail s)
    | Some (('.' | ')') as c) ->
        let s = Sub.tail s in
        if is_empty s then Llist_item (Ordered (m, c), n + 1 + ind, s)
        else
          let ind' = indent s in
          if ind' = 0 then raise Fail;
          let ind' = if ind' <= 4 then ind' else 1 in
          Llist_item (Ordered (m, c), n + ind + ind' + 1, Sub.offset ind' s)
    | Some _ | None -> raise Fail
  in
  loop 0 0 s

let tag_name s0 =
  match Sub.head s0 with
  | Some ('a' .. 'z' | 'A' .. 'Z') ->
      let rec loop len s =
        match Sub.head s with
        | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-') ->
            loop (succ len) (Sub.tail s)
        | Some _ | None -> (Sub.to_string (Sub.sub s0 ~len), s)
      in
      loop 1 (Sub.tail s0)
  | Some _ | None -> raise Fail

let known_tags =
  [ "address"
  ; "aside"
  ; "base"
  ; "basefont"
  ; "blockquote"
  ; "body"
  ; "caption"
  ; "center"
  ; "col"
  ; "colgroup"
  ; "dd"
  ; "details"
  ; "dialog"
  ; "dir"
  ; "div"
  ; "dl"
  ; "dt"
  ; "fieldset"
  ; "figcaption"
  ; "figure"
  ; "footer"
  ; "form"
  ; "frame"
  ; "frameset"
  ; "h1"
  ; "h2"
  ; "h3"
  ; "h4"
  ; "h5"
  ; "h6"
  ; "head"
  ; "header"
  ; "hr"
  ; "html"
  ; "iframe"
  ; "legend"
  ; "li"
  ; "link"
  ; "main"
  ; "menu"
  ; "menuitem"
  ; "meta"
  ; "nav"
  ; "noframes"
  ; "ol"
  ; "optgroup"
  ; "option"
  ; "p"
  ; "param"
  ; "section"
  ; "source"
  ; "summary"
  ; "table"
  ; "tbody"
  ; "td"
  ; "tfoot"
  ; "th"
  ; "thead"
  ; "title"
  ; "tr"
  ; "track"
  ; "ul"
  ]

let special_tags = [ "pre"; "script"; "style"; "textarea" ]

let known_tag s =
  let s = String.lowercase_ascii s in
  List.mem s known_tags

let special_tag s =
  let s = String.lowercase_ascii s in
  List.mem s special_tags

let closing_tag s =
  let s = trim_ws s in
  match Sub.head s with
  | Some '>' ->
      if not (is_empty (Sub.tail s)) then raise Fail;
      Lhtml (false, Hblank)
  | Some _ | None -> raise Fail

let special_tag tag s =
  if not (special_tag tag) then raise Fail;
  match Sub.head s with
  | Some (' ' | '\t' | '\010' .. '\013' | '>') | None ->
      Lhtml (true, Hcontains [ "</script>"; "</pre>"; "</style>" ])
  | Some _ -> raise Fail

let known_tag tag s =
  if not (known_tag tag) then raise Fail;
  match Sub.take 2 s with
  | (' ' | '\t' | '\010' .. '\013') :: _ | [] | '>' :: _ | '/' :: '>' :: _ ->
      Lhtml (true, Hblank)
  | _ -> raise Fail

let ws1 s =
  match Sub.head s with
  | Some (' ' | '\t' | '\010' .. '\013') -> trim_ws s
  | Some _ | None -> raise Fail

let attribute_name s =
  match Sub.head s with
  | Some ('a' .. 'z' | 'A' .. 'Z' | '_' | ':') ->
      let rec loop s =
        match Sub.head s with
        | Some ('a' .. 'z' | 'A' .. 'Z' | '_' | '.' | ':' | '0' .. '9') ->
            loop (Sub.tail s)
        | Some _ | None -> s
      in
      loop s
  | Some _ | None -> raise Fail

let attribute_value s =
  match Sub.head s with
  | Some (('\'' | '"') as c) ->
      let rec loop s =
        match Sub.head s with
        | Some c1 when c = c1 -> Sub.tail s
        | Some _ -> loop (Sub.tail s)
        | None -> raise Fail
      in
      loop (Sub.tail s)
  | Some _ ->
      let rec loop first s =
        match Sub.head s with
        | Some
            (' ' | '\t' | '\010' .. '\013' | '"' | '\'' | '=' | '<' | '>' | '`')
        | None ->
            if first then raise Fail;
            s
        | Some _ -> loop false (Sub.tail s)
      in
      loop true s
  | None -> raise Fail

let attribute s =
  let s = ws1 s in
  let s = attribute_name s in
  let s = trim_ws s in
  match Sub.head s with
  | Some '=' ->
      let s = trim_ws (Sub.tail s) in
      attribute_value s
  | Some _ | None -> s

let attributes s =
  let rec loop s = match attribute s with s -> loop s | exception Fail -> s in
  loop s

let open_tag s =
  let s = attributes s in
  let s = trim_ws s in
  let n =
    match Sub.take 2 s with
    | '/' :: '>' :: _ -> 2
    | '>' :: _ -> 1
    | _ -> raise Fail
  in
  if not (is_empty (Sub.drop n s)) then raise Fail;
  Lhtml (false, Hblank)

let raw_html s =
  match Sub.take 10 s with
  | '<' :: '?' :: _ -> Lhtml (true, Hcontains [ "?>" ])
  | '<' :: '!' :: '-' :: '-' :: _ -> Lhtml (true, Hcontains [ "-->" ])
  | '<' :: '!' :: '[' :: 'C' :: 'D' :: 'A' :: 'T' :: 'A' :: '[' :: _ ->
      Lhtml (true, Hcontains [ "]]>" ])
  | '<' :: '!' :: _ -> Lhtml (true, Hcontains [ ">" ])
  | '<' :: '/' :: _ ->
      let tag, s = tag_name (Sub.drop 2 s) in
      (known_tag tag ||| closing_tag) s
  | '<' :: _ ->
      let tag, s = tag_name (Sub.drop 1 s) in
      (special_tag tag ||| known_tag tag ||| open_tag) s
  | _ -> raise Fail

let blank s =
  if not (is_empty s) then raise Fail;
  Lempty

let tag_string s =
  let buf = Buffer.create 17 in
  let s, a =
    match Sub.last s with Some '}' -> attribute_string s | _ -> (s, [])
  in
  let s = trim_ws ~rev:true (trim_ws s) in
  let rec loop s =
    match Sub.head s with
    | Some (' ' | '\t' | '\010' .. '\013') | None -> (Buffer.contents buf, a)
    | Some c ->
        Buffer.add_char buf c;
        loop (Sub.tail s)
  in
  loop (trim_ws s)

let def_list s =
  let s = Sub.tail s in
  match Sub.head s with
  | Some (' ' | '\t' | '\010' .. '\013') ->
      Ldef_list (String.trim (Sub.to_string s))
  | _ -> raise Fail

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
  | Some '=' -> setext_heading s
  | Some '-' ->
      (setext_heading ||| thematic_break ||| unordered_list_item ind) s
  | Some '_' -> thematic_break s
  | Some '#' -> atx_heading s
  | Some ('~' | '`') -> fenced_code ind s
  | Some '<' -> raw_html s
  | Some '*' -> (thematic_break ||| unordered_list_item ind) s
  | Some '+' -> unordered_list_item ind s
  | Some '0' .. '9' -> ordered_list_item ind s
  | Some ':' -> def_list s
  | Some _ -> (blank ||| indented_code ind) s
  | None -> Lempty

let parse s = try parse s with Fail -> Lparagraph

open P

let is_empty st =
  let off = pos st in
  try
    let rec loop () =
      match next st with
      | ' ' | '\t' | '\010' .. '\013' -> loop ()
      | _ ->
          set_pos st off;
          false
    in
    loop ()
  with Fail ->
    set_pos st off;
    true

let inline_attribute_string s =
  let ppos = pos s in
  ws s;
  let a =
    match peek s with
    | Some '{' ->
        let buf = Buffer.create 64 in
        let rec loop s pos =
          match peek s with
          | Some '}' ->
              junk s;
              Some (Buffer.contents buf)
          | None | Some '{' ->
              set_pos s pos;
              None
          | Some c ->
              Buffer.add_char buf c;
              junk s;
              loop s pos
        in
        junk s;
        loop s (pos s)
    | _ -> None
  in
  let attr = parse_attributes a in
  if attr = [] then set_pos s ppos;
  attr

let entity buf st =
  junk st;
  match on_sub entity st with
  | cs -> List.iter (Buffer.add_utf_8_uchar buf) cs
  | exception Fail -> Buffer.add_char buf '&'

module Pre = struct
  type delim =
    | Ws
    | Punct
    | Other

  type emph_style =
    | Star
    | Underscore

  type link_kind =
    | Img
    | Url

  type t =
    | Bang_left_bracket
    | Left_bracket of link_kind
    | Emph of delim * delim * emph_style * int
    | R of attributes inline

  let concat = function [ x ] -> x | l -> Concat ([], l)

  let left_flanking = function
    | Emph (_, Other, _, _) | Emph ((Ws | Punct), Punct, _, _) -> true
    | _ -> false

  let right_flanking = function
    | Emph (Other, _, _, _) | Emph (Punct, (Ws | Punct), _, _) -> true
    | _ -> false

  let is_opener = function
    | Emph (pre, _, Underscore, _) as x ->
        left_flanking x && ((not (right_flanking x)) || pre = Punct)
    | Emph (_, _, Star, _) as x -> left_flanking x
    | _ -> false

  let is_closer = function
    | Emph (_, post, Underscore, _) as x ->
        right_flanking x && ((not (left_flanking x)) || post = Punct)
    | Emph (_, _, Star, _) as x -> right_flanking x
    | _ -> false

  let classify_delim = function
    | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
    | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\'
    | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' ->
        Punct
    | ' ' | '\t' | '\010' .. '\013' | '\160' -> Ws
    | _ -> Other

  let to_r = function
    | Bang_left_bracket -> Text ([], "![")
    | Left_bracket Img -> Text ([], "![")
    | Left_bracket Url -> Text ([], "[")
    | Emph (_, _, Star, n) -> Text ([], String.make n '*')
    | Emph (_, _, Underscore, n) -> Text ([], String.make n '_')
    | R x -> x

  let rec find_next_emph = function
    | Emph (pre, post, style, n) :: _ -> Some (pre, post, style, n)
    | _ :: xs -> find_next_emph xs
    | [] -> None

  let rec find_next_closer_emph = function
    | (Emph (pre, post, style, n) as e) :: _ when is_closer e ->
        Some (pre, post, style, n)
    | _ :: xs -> find_next_closer_emph xs
    | [] -> None

  (* Checks the lengths of two different emphasis delimiters to see if there can be a match.

     From the spec: "If one of the delimiters can both open and close emphasis, then the sum of the lengths
       of the delimiter runs containing the opening and closing delimiters must not be
       a multiple of 3 unless both lengths are multiples of 3" *)
  let is_emph_match n1 n2 =
    (*
      - *foo**bar**baz*

            *foo** -> the second delimiter ** is both an opening and closing delimiter.
                      The sum of the length of both delimiters is 3, so they can't be matched.

            **bar** -> they are both opening and closing delemiters.
                       Their sum is 4 which is not a multiple of 3 so they can be matched to produce <strong>bar</strong>

            The end result is: <em>foo<strong>bar</strong>baz</em>

      - *foo***bar**baz*

            *foo*** -> *** is both an opening and closing delimiter.
                       Their sum is 4 so they can be matched to produce: <em>foo</em>**

            **bar** -> they are both opening and closing delemiters.
                       Their sum is 4 which is not a multiple of 3 so they can be matched to produce <strong>bar</strong>

            The end result is: <em>foo</em><strong>bar</strong>baz*

      - ***foo***bar**baz*

            ***foo*** -> the second delimiter *** is both an opening and closing delimiter.
                         Their sum is 6 which is a multiple of 3. However, both lengths are multiples of 3
                         so they can be matched to produce: <em><strong>foo</strong></em>

            bar**baz* -> ** is both an opening and closing delimiter.
                         Their sum is 3 so they can't be matched

            The end result is: <em><strong>foo</strong></em>bar**baz*
      *)
    if (n1 + n2) mod 3 = 0 && n1 mod 3 != 0 && n2 mod 3 != 0 then false
    else true

  let rec parse_emph = function
    | (Emph (pre, _, q1, n1) as x1) :: xs when is_opener x1 ->
        let rec loop acc = function
          | (Emph (_, post, q2, n2) as x2) :: xs1 as xs
            when is_closer x2 && q1 = q2 ->
              (* At this point we have an openener followed by a closer. Both are of the same style (either * or _) *)
              if (is_opener x2 || is_closer x1) && not (is_emph_match n1 n2)
              then
                (*
                 The second delimiter (the closer) is also an opener, and both delimiters don't match together,
                 according to the "mod 3" rule. In that case, we check if the next delimiter can match.

                   *foo**bar**baz*  The second delimiter that's both an opener/closer ( ** before bar)
                                    matches with the next delimiter ( ** after bar). They'll become
                                    <strong>bar</strong>. The end result will be: <em>foo<strong>bar</strong>baz</em>


                   *foo**bar*baz*  The second delimiter that's both an opener/closer ( ** before bar)
                                   doesn't match with the next delimiter ( * after bar). **bar will be
                                   considered as regular text. The end result will be: <em>foo**bar</em>baz*
                 *)
                match find_next_emph xs1 with
                | Some (_, _, _, n3) when is_emph_match n3 n2 ->
                    let xs' = parse_emph xs in
                    loop acc xs'
                | _ -> loop (x2 :: acc) xs1
              else
                let xs =
                  if n1 >= 2 && n2 >= 2 then
                    if n2 > 2 then Emph (Other, post, q2, n2 - 2) :: xs1
                    else xs1
                  else if n2 > 1 then Emph (Punct, post, q2, n2 - 1) :: xs1
                  else xs1
                in
                let r =
                  let il = concat (List.map to_r (List.rev acc)) in
                  if n1 >= 2 && n2 >= 2 then R (Strong ([], il)) :: xs
                  else R (Emph ([], il)) :: xs
                in
                let r =
                  if n1 >= 2 && n2 >= 2 then
                    if n1 > 2 then Emph (pre, Other, q1, n1 - 2) :: r else r
                  else if n1 > 1 then Emph (pre, Punct, q1, n1 - 1) :: r
                  else r
                in
                parse_emph r
          | (Emph (_, _, q2, _) as x2) :: xs1 as xs when is_opener x2 ->
              (*
               This case happens when we encounter a second opener delimiter. We look ahead for the next closer,
               and if the next closer is of the same style, we can match them together.

                 *foo _bar_ baz_  The second opener (_ before `bar`) is of the same style as the next closer
                                  (_ after `bar`). We can match them to produce <em>bar</em>
                                  The end result will be: *foo <em>bar</em> baz_


                 *foo _bar* baz_  The second opener (_ before `bar`) is not of the same style as the next closer
                                  ( * after `bar`). They can't be matched so we'll consider _bar as regular text.
                                  The end result will be: <em>foo _bar</em> baz_
               *)
              let is_next_closer_same =
                match find_next_closer_emph xs1 with
                | None -> false
                | Some (_, _, q3, _) -> q2 = q3
              in
              if not is_next_closer_same then loop (x2 :: acc) xs1
              else loop acc (parse_emph xs)
          | x :: xs -> loop (x :: acc) xs
          | [] -> x1 :: List.rev acc
        in
        loop [] xs
    | x :: xs -> x :: parse_emph xs
    | [] -> []

  let parse_emph xs = concat (List.map to_r (parse_emph xs))
end

let escape buf st =
  if next st <> '\\' then raise Fail;
  match peek st with
  | Some c when is_punct c ->
      junk st;
      Buffer.add_char buf c
  | Some _ | None -> Buffer.add_char buf '\\'

let link_label allow_balanced_brackets st =
  if peek_exn st <> '[' then raise Fail;
  junk st;
  let buf = Buffer.create 17 in
  let rec loop n nonempty =
    match peek_exn st with
    | ']' when n = 0 ->
        junk st;
        if not nonempty then raise Fail;
        Buffer.contents buf
    | ']' as c ->
        assert (n > 0);
        junk st;
        Buffer.add_char buf c;
        loop (pred n) true
    | '\\' as c ->
        junk st;
        Buffer.add_char buf c;
        begin
          match peek_exn st with
          | c when is_punct c ->
              junk st;
              Buffer.add_char buf c
          | _ -> ()
        end;
        loop n true
    | '[' when not allow_balanced_brackets -> raise Fail
    | '[' as c ->
        junk st;
        Buffer.add_char buf c;
        loop (succ n) true
    | (' ' | '\t' | '\010' .. '\013') as c ->
        junk st;
        Buffer.add_char buf c;
        loop n nonempty
    | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop n true
  in
  loop 0 false

type add_uchar_result =
  { start : bool
  ; seen_ws : bool
  }

(* based on https://erratique.ch/software/uucp/doc/Uucp/Case/index.html#caselesseq *)
let normalize s =
  let canonical_caseless_key s =
    let b = Buffer.create (String.length s * 2) in
    let to_nfd_and_utf_8 =
      let n = Uunf.create `NFD in
      let rec add v =
        match Uunf.add n v with
        | `Await | `End -> ()
        | `Uchar u ->
            Uutf.Buffer.add_utf_8 b u;
            add `Await
      in
      add
    in
    let add_nfd =
      let n = Uunf.create `NFD in
      let rec add v =
        match Uunf.add n v with
        | `Await | `End -> ()
        | `Uchar u ->
            (match Uucp.Case.Fold.fold u with
            | `Self -> to_nfd_and_utf_8 (`Uchar u)
            | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us);
            add `Await
      in
      add
    in
    let uspace = `Uchar (Uchar.of_char ' ') in
    let add_uchar { start; seen_ws } _ = function
      | `Malformed _ ->
          add_nfd (`Uchar Uutf.u_rep);
          { start = false; seen_ws = false }
      | `Uchar u as uchar ->
          if Uucp.White.is_white_space u then { start; seen_ws = true }
          else (
            if (not start) && seen_ws then add_nfd uspace;
            add_nfd uchar;
            { start = false; seen_ws = false })
    in
    let (_ : add_uchar_result) =
      Uutf.String.fold_utf_8 add_uchar { start = true; seen_ws = false } s
    in
    add_nfd `End;
    to_nfd_and_utf_8 `End;
    Buffer.contents b
  in
  canonical_caseless_key s

let tag_name st =
  match peek_exn st with
  | 'a' .. 'z' | 'A' .. 'Z' ->
      junk st;
      let rec loop () =
        match peek st with
        | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-') ->
            junk st;
            loop ()
        | Some _ | None -> ()
      in
      loop ()
  | _ -> raise Fail

let ws_buf buf st =
  let rec loop () =
    match peek st with
    | Some ((' ' | '\t' | '\010' .. '\013') as c) ->
        Buffer.add_char buf c;
        junk st;
        loop ()
    | Some _ | None -> ()
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
    match protect p st with () -> loop () | exception Fail -> ()
  in
  loop ()

let single_quoted_attribute st =
  if next st <> '\'' then raise Fail;
  let rec loop () =
    match peek_exn st with
    | '\'' -> junk st
    (* | '&' -> *)
    (*     entity buf st; loop () *)
    | _ ->
        junk st;
        loop ()
  in
  loop ()

let double_quoted_attribute st =
  if next st <> '"' then raise Fail;
  let rec loop () =
    match peek_exn st with
    | '"' -> junk st
    (* | '&' -> *)
    (*     entity buf st; loop () *)
    | _ ->
        junk st;
        loop ()
  in
  loop ()

let unquoted_attribute st =
  let rec loop n =
    match peek_exn st with
    | ' ' | '\t' | '\010' .. '\013' | '"' | '\'' | '=' | '<' | '>' | '`' ->
        if n = 0 then raise Fail
    (* | '&' -> *)
    (*     entity buf st; loop () *)
    | _ ->
        junk st;
        loop (succ n)
  in
  loop 0

let attribute_value st =
  match peek_exn st with
  | '\'' -> single_quoted_attribute st
  | '"' -> double_quoted_attribute st
  | _ -> unquoted_attribute st

let attribute_name st =
  match peek_exn st with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | ':' ->
      junk st;
      let rec loop () =
        match peek st with
        | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | ':' | '-') ->
            junk st;
            loop ()
        | Some _ | None -> ()
      in
      loop ()
  | _ -> raise Fail

let option d p st = match protect p st with r -> r | exception Fail -> d
let some p st = Some (p st)
let attribute_value_specification = ws >>> char '=' >>> ws >>> attribute_value

let ws1_buf buf st =
  match peek st with
  | Some (' ' | '\t' | '\010' .. '\013') -> ws_buf buf st
  | Some _ | None -> raise Fail

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
  begin
    match peek_exn st with
    | '/' -> junk st
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
    match peek_exn st with
    | '-' as c -> (
        junk st;
        match peek_exn st with
        | '-' ->
            junk st;
            if next st <> '>' then raise Fail;
            Buffer.add_string buf "-->";
            Buffer.contents buf
        | '>' when start -> raise Fail
        | _ ->
            Buffer.add_char buf c;
            loop false)
    | '>' when start -> raise Fail
    | '&' ->
        entity buf st;
        loop false
    | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop false
  in
  loop true

let processing_instruction st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '?' then raise Fail;
  Buffer.add_string buf "<?";
  let rec loop () =
    match peek_exn st with
    | '?' as c -> (
        junk st;
        match peek_exn st with
        | '>' ->
            junk st;
            Buffer.add_string buf "?>";
            Buffer.contents buf
        | _ ->
            Buffer.add_char buf c;
            loop ())
    | '&' ->
        entity buf st;
        loop ()
    | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop ()
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
    match peek_exn st with
    | ']' as c -> (
        junk st;
        match peek_exn st with
        | ']' as c1 -> (
            junk st;
            match peek_exn st with
            | '>' ->
                junk st;
                Buffer.add_string buf "]]>";
                Buffer.contents buf
            | _ ->
                Buffer.add_char buf c;
                Buffer.add_char buf c1;
                loop ())
        | _ ->
            Buffer.add_char buf c;
            loop ())
    | '&' ->
        entity buf st;
        loop ()
    | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop ()
  in
  loop ()

let declaration st =
  let buf = Buffer.create 17 in
  if next st <> '<' then raise Fail;
  if next st <> '!' then raise Fail;
  Buffer.add_string buf "<!";
  match peek_exn st with
  | 'A' .. 'Z' ->
      let rec loop () =
        match peek_exn st with
        | 'A' .. 'Z' as c ->
            junk st;
            Buffer.add_char buf c;
            loop ()
        | ' ' | '\t' | '\010' .. '\013' ->
            ws1_buf buf st;
            let rec loop () =
              match peek_exn st with
              | '>' as c ->
                  junk st;
                  Buffer.add_char buf c;
                  Buffer.contents buf
              | '&' ->
                  entity buf st;
                  loop ()
              | _ as c ->
                  junk st;
                  Buffer.add_char buf c;
                  loop ()
            in
            loop ()
        | _ -> raise Fail
      in
      loop ()
  | _ -> raise Fail

let link_destination st =
  let buf = Buffer.create 17 in
  match peek_exn st with
  | '<' ->
      junk st;
      let rec loop () =
        match peek_exn st with
        | '>' ->
            junk st;
            Buffer.contents buf
        | '\010' .. '\013' | '<' -> raise Fail
        | '\\' ->
            escape buf st;
            loop ()
        | '&' ->
            entity buf st;
            loop ()
        | _ as c ->
            junk st;
            Buffer.add_char buf c;
            loop ()
      in
      loop ()
  | _ ->
      let rec loop n =
        match peek st with
        | Some ('(' as c) ->
            junk st;
            Buffer.add_char buf c;
            loop (succ n)
        | Some ')' when n = 0 ->
            if Buffer.length buf = 0 then raise Fail;
            Buffer.contents buf
        | Some (')' as c) ->
            junk st;
            Buffer.add_char buf c;
            loop (pred n)
        | Some '\\' ->
            escape buf st;
            loop n
        | Some '&' ->
            entity buf st;
            loop n
        | Some (' ' | '\t' | '\x00' .. '\x1F' | '\x7F') | None ->
            if n > 0 || Buffer.length buf = 0 then raise Fail;
            Buffer.contents buf
        | Some c ->
            junk st;
            Buffer.add_char buf c;
            loop n
      in
      loop 0

let eol st =
  match peek st with Some '\n' -> junk st | Some _ -> raise Fail | None -> ()

let link_title st =
  let buf = Buffer.create 17 in
  match peek_exn st with
  | ('\'' | '"') as c ->
      junk st;
      let rec loop () =
        match peek_exn st with
        | '\\' ->
            escape buf st;
            loop ()
        | '&' ->
            entity buf st;
            loop ()
        | _ as c1 when c = c1 ->
            junk st;
            Buffer.contents buf
        | _ as c1 ->
            junk st;
            Buffer.add_char buf c1;
            loop ()
      in
      loop ()
  | '(' ->
      junk st;
      let rec loop () =
        match peek_exn st with
        | '\\' ->
            escape buf st;
            loop ()
        | '&' ->
            entity buf st;
            loop ()
        | ')' ->
            junk st;
            Buffer.contents buf
        | _ as c ->
            junk st;
            Buffer.add_char buf c;
            loop ()
      in
      loop ()
  | _ -> raise Fail

let space st = match peek_exn st with ' ' -> junk st | _ -> raise Fail

let many p st =
  try
    while true do
      p st
    done
  with Fail -> ()

let scheme st =
  match peek_exn st with
  | 'a' .. 'z' | 'A' .. 'Z' ->
      let rec loop n =
        if n < 32 then
          match peek st with
          | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '.' | '-') ->
              junk st;
              loop (succ n)
          | Some _ | None -> n
        else n
      in
      let n = loop 0 in
      if n < 2 then raise Fail
  | _ -> raise Fail

let absolute_uri st =
  let p = pos st in
  scheme st;
  if next st <> ':' then raise Fail;
  let rec loop () =
    match peek st with
    | Some
        ( ' ' | '\t'
        | '\010' .. '\013'
        | '\x00' .. '\x1F'
        | '\x7F' .. '\x9F'
        | '<' | '>' )
    | None ->
        let txt = range st p (pos st - p) in
        (txt, txt)
    | Some _ ->
        junk st;
        loop ()
  in
  loop ()

let email_address st =
  let p = pos st in
  let rec loop n =
    match peek_exn st with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '.' | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '/' | '=' | '?'
    | '^' | '_' | '`' | '{' | '|' | '}' | '~' | '-' ->
        junk st;
        loop (succ n)
    | '@' ->
        junk st;
        let label st =
          let let_dig st =
            match peek_exn st with
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
                junk st;
                false
            | '-' ->
                junk st;
                true
            | _ -> raise Fail
          in
          if let_dig st then raise Fail;
          let rec loop last =
            match let_dig st with
            | r -> loop r
            | exception Fail -> if last then raise Fail
          in
          loop false
        in
        label st;
        list (char '.' >>> label) st;
        let txt = range st p (pos st - p) in
        (txt, "mailto:" ^ txt)
    | _ -> raise Fail
  in
  loop 0

let autolink st =
  match peek_exn st with
  | '<' ->
      junk st;
      let label, destination = (absolute_uri ||| email_address) st in
      if next st <> '>' then raise Fail;
      { Ast.label = Text ([], label); destination; title = None }
  | _ -> raise Fail

let inline_link =
  char '('
  >>> ws
  >>> option
        ("", None)
        (pair link_destination (option None (ws1 >>> some link_title)))
  <<< ws
  <<< char ')'

let get_buf buf =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  s

let text buf acc =
  if Buffer.length buf = 0 then acc else Pre.R (Text ([], get_buf buf)) :: acc

let inline_pre buf acc st =
  let pos = pos st in
  let rec gobble_open_backtick n =
    match peek st with
    | Some '`' ->
        junk st;
        gobble_open_backtick (succ n)
    | Some _ ->
        let acc = text buf acc in
        let bufcode = Buffer.create 17 in
        let finish () =
          let content = Buffer.contents bufcode in
          let content =
            if String.for_all (fun c -> c = ' ') content then content
            else if
              String.length content >= 2
              && content.[0] = ' '
              && content.[String.length content - 1] = ' '
            then String.sub content 1 (String.length content - 2)
            else content
          in
          let attr = inline_attribute_string st in
          Pre.R (Code (attr, content)) :: acc
        in
        let rec gobble_body start m =
          match peek st with
          | Some '`' ->
              junk st;
              gobble_body start (succ m)
          | _ when m = n -> finish ()
          | Some ((' ' | '\t' | '\010' .. '\013') as c) ->
              if m > 0 then Buffer.add_string bufcode (String.make m '`');
              Buffer.add_char bufcode (if c = '\010' then ' ' else c);
              junk st;
              gobble_body (start && m = 0) 0
          | Some c ->
              junk st;
              (* if seen_ws then Buffer.add_char bufcode ' '; *)
              if m > 0 then Buffer.add_string bufcode (String.make m '`');
              Buffer.add_char bufcode c;
              gobble_body false 0
          | None ->
              Buffer.add_string buf (range st pos n);
              set_pos st (pos + n);
              acc
        in
        gobble_body true 0
    | None ->
        Buffer.add_string buf (String.make n '`');
        acc
  in
  gobble_open_backtick 0

let rec inline defs st =
  let buf = Buffer.create 0 in
  let text acc = text buf acc in
  let rec reference_link kind acc st =
    let off0 = pos st in
    match protect (link_label true) st with
    | lab -> (
        let reflink lab =
          let s = normalize lab in
          match
            List.find_opt
              (fun ({ label; _ } : attributes link_def) -> label = s)
              defs
          with
          | Some { label = _; destination; title; attributes = attr } ->
              let lab1 = inline defs (of_string lab) in
              let r =
                let def = { label = lab1; destination; title } in
                match kind with
                | Pre.Img -> Image (attr, def)
                | Url -> Link (attr, def)
              in
              loop (Pre.R r :: text acc) st
          | None ->
              if kind = Img then Buffer.add_char buf '!';
              Buffer.add_char buf '[';
              let acc = text acc in
              set_pos st (succ off0);
              loop acc st
        in
        match peek st with
        | Some '[' -> (
            if peek_after '\000' st = ']' then (
              junk st;
              junk st;
              reflink lab)
            else
              match protect (link_label false) st with
              | _ ->
                  set_pos st off0;
                  junk st;
                  loop (Left_bracket kind :: text acc) st
              | exception Fail -> reflink lab)
        | Some '(' -> (
            match protect inline_link st with
            | _ ->
                set_pos st off0;
                junk st;
                loop (Left_bracket kind :: text acc) st
            | exception Fail -> reflink lab)
        | Some _ | None -> reflink lab)
    | exception Fail ->
        junk st;
        loop (Left_bracket kind :: text acc) st
  and loop ~seen_link acc st =
    match peek_exn st with
    | '<' as c -> (
        match protect autolink st with
        | def ->
            let attr = inline_attribute_string st in
            loop ~seen_link (Pre.R (Link (attr, def)) :: text acc) st
        | exception Fail -> (
            match
              protect
                (closing_tag
                ||| open_tag
                ||| html_comment
                ||| declaration
                ||| cdata_section
                ||| processing_instruction)
                st
            with
            | tag -> loop ~seen_link (Pre.R (Html ([], tag)) :: text acc) st
            | exception Fail ->
                junk st;
                Buffer.add_char buf c;
                loop ~seen_link acc st))
    | '\n' ->
        junk st;
        sp st;
        loop ~seen_link (Pre.R (Soft_break []) :: text acc) st
    | ' ' as c -> (
        junk st;
        match peek st with
        | Some ' ' -> (
            match protect (many space >>> char '\n' >>> many space) st with
            | () -> loop ~seen_link (Pre.R (Hard_break []) :: text acc) st
            | exception Fail ->
                junk st;
                Buffer.add_string buf "  ";
                loop ~seen_link acc st)
        | Some '\n' -> loop ~seen_link acc st
        | Some _ | None ->
            Buffer.add_char buf c;
            loop ~seen_link acc st)
    | '`' -> loop ~seen_link (inline_pre buf acc st) st
    | '\\' as c -> (
        junk st;
        match peek st with
        | Some '\n' ->
            junk st;
            loop ~seen_link (Pre.R (Hard_break []) :: text acc) st
        | Some c when is_punct c ->
            junk st;
            Buffer.add_char buf c;
            loop ~seen_link acc st
        | Some _ | None ->
            Buffer.add_char buf c;
            loop ~seen_link acc st)
    | '!' as c -> (
        junk st;
        match peek st with
        | Some '[' -> reference_link ~seen_link Img (text acc) st
        | Some _ | None ->
            Buffer.add_char buf c;
            loop ~seen_link acc st)
    | '&' ->
        entity buf st;
        loop ~seen_link acc st
    | ']' ->
        junk st;
        let acc = text acc in
        let rec aux ~seen_link xs = function
          | Pre.Left_bracket Url :: acc' when seen_link ->
              Buffer.add_char buf ']';
              let acc' = List.rev_append (Pre.R (Text ([], "[")) :: xs) acc' in
              loop ~seen_link acc' st
          | Left_bracket k :: acc' -> (
              match peek st with
              | Some '(' -> (
                  match protect inline_link st with
                  | destination, title ->
                      let attr = inline_attribute_string st in
                      let r =
                        let label = Pre.parse_emph xs in
                        let def = { label; destination; title } in
                        match k with
                        | Img -> Image (attr, def)
                        | Url -> Link (attr, def)
                      in
                      loop ~seen_link (Pre.R r :: acc') st
                  | exception Fail ->
                      Buffer.add_char buf ']';
                      loop ~seen_link acc st)
              | Some '[' -> (
                  let label = Pre.parse_emph xs in
                  let off1 = pos st in
                  match link_label false st with
                  | lab -> (
                      let s = normalize lab in
                      match
                        List.find_opt
                          (fun ({ label; _ } : attributes link_def) ->
                            label = s)
                          defs
                      with
                      | Some
                          { label = _; destination; title; attributes = attr }
                        ->
                          let def = { label; destination; title } in
                          let r =
                            match k with
                            | Img -> Image (attr, def)
                            | Url -> Link (attr, def)
                          in
                          loop ~seen_link (Pre.R r :: acc') st
                      | None ->
                          if k = Img then Buffer.add_char buf '!';
                          Buffer.add_char buf '[';
                          let acc = Pre.R label :: text acc' in
                          Buffer.add_char buf ']';
                          set_pos st off1;
                          loop ~seen_link acc st)
                  | exception Fail ->
                      if k = Img then Buffer.add_char buf '!';
                      Buffer.add_char buf '[';
                      let acc = Pre.R label :: text acc in
                      Buffer.add_char buf ']';
                      set_pos st off1;
                      loop ~seen_link acc st)
              | Some _ | None ->
                  Buffer.add_char buf ']';
                  loop ~seen_link acc st)
          | (Pre.R (Link _) as x) :: acc' -> aux ~seen_link:true (x :: xs) acc'
          | x :: acc' -> aux ~seen_link (x :: xs) acc'
          | [] ->
              Buffer.add_char buf ']';
              loop ~seen_link acc st
        in
        aux ~seen_link [] acc
    | '[' -> reference_link ~seen_link Url acc st
    | ('*' | '_') as c ->
        let pre = peek_before ' ' st in
        let f post n st =
          let pre = pre |> Pre.classify_delim in
          let post = post |> Pre.classify_delim in
          let e = if c = '*' then Pre.Star else Pre.Underscore in
          loop ~seen_link (Pre.Emph (pre, post, e, n) :: text acc) st
        in
        let rec aux n =
          match peek st with
          | Some c1 when c1 = c ->
              junk st;
              aux (succ n)
          | Some c1 -> f c1 n st
          | None -> f ' ' n st
        in
        aux 0
    | _ as c ->
        junk st;
        Buffer.add_char buf c;
        loop ~seen_link acc st
    | exception Fail -> Pre.parse_emph (List.rev (text acc))
  in
  loop ~seen_link:false [] st

let sp3 st =
  match peek_exn st with
  | ' ' -> (
      junk st;
      match peek_exn st with
      | ' ' -> (
          junk st;
          match peek_exn st with
          | ' ' ->
              junk st;
              3
          | _ -> 2
          | exception Fail -> 2)
      | _ -> 1
      | exception Fail -> 1)
  | _ -> 0
  | exception Fail -> 0

let link_reference_definition st : attributes Ast.link_def =
  let ws st =
    let rec loop seen_nl =
      match peek st with
      | Some (' ' | '\t' | '\011' .. '\013') ->
          junk st;
          loop seen_nl
      | Some '\n' when not seen_nl ->
          junk st;
          loop true
      | Some _ | None -> ()
    in
    loop false
  in
  let ws1 st =
    match next st with
    | ' ' | '\t' | '\010' .. '\013' -> ws st
    | _ -> raise Fail
  in
  ignore (sp3 st);
  let label = link_label false st in
  if next st <> ':' then raise Fail;
  ws st;
  let destination = link_destination st in
  let attributes = inline_attribute_string st in
  match protect (ws1 >>> link_title <<< sp <<< eol) st with
  | title -> { label; destination; title = Some title; attributes }
  | exception Fail ->
      (sp >>> eol) st;
      { label; destination; title = None; attributes }

let link_reference_definitions st =
  let rec loop acc =
    match protect link_reference_definition st with
    | def -> loop (def :: acc)
    | exception Fail -> (acc, pos st)
  in
  loop []

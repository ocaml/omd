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

let rec ws ?rev s =
  match Sub.head ?rev s with
  | Some (' ' | '\t' | '\010'..'\013') -> ws ?rev (Sub.tail ?rev s)
  | None | Some _ -> s

let is_empty s =
  Sub.is_empty (ws s)

let thematic_break c s =
  let rec loop n s =
    match Sub.head s with
    | Some c1 when c = c1 ->
        loop (succ n) (ws (Sub.tail s))
    | Some _ ->
        false
    | None ->
        n >= 3
  in
  loop 0 s

let setext_heading s =
  match Sub.head s with
  | Some ('-' | '=' as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 ->
            loop (succ n) (Sub.tail s)
        | Some _ ->
            if Sub.is_empty (ws s) then Some n else None
        | None ->
            Some n
      in
      loop 1 (Sub.tail s)
  | Some _ | None ->
      None

let atx_heading s =
  let rec loop n s =
    if n > 6 then None
    else begin
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
          Some (n, loop s)
      | Some _ ->
          None
      | None ->
          Some (n, s)
    end
  in
  loop 0 s

let split_first s =
  let s = ws s in
  let rec loop len t =
    match Sub.head t with
    | Some (' ' | '\t' | '\010'..'\013') | None ->
        Sub.sub ~len s, t
    | Some _ ->
        loop (succ len) (Sub.tail t)
  in
  loop 0 s

let code_block s =
  match Sub.head s with
  | Some ('`' | '~' as c) ->
      let rec loop n s =
        match Sub.head s with
        | Some c1 when c = c1 ->
            loop (succ n) (Sub.tail s)
        | Some _ | None ->
            if n < 3 then None
            else begin
              let s, t = split_first s in
              let f = function '`' -> true | _ -> false in
              if c = '`' && (Sub.exists f s || Sub.exists f t) then
                None
              else
                Some (n, s)
            end
      in
      loop 1 (Sub.tail s)
  | Some _ | None ->
      None

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

let unordered_list_item s =
  match Sub.head s with
  | Some ('+' | '-' | '*' as c) ->
      let s = Sub.tail s in
      if is_empty s then
        Some (Unordered c, 2, s)
      else
        let n = indent s in
        if n = 0 then None
        else begin
          let n = if n <= 4 then n else 1 in
          Some (Unordered c, n + 1, Sub.offset n s)
        end
  | Some _ | None ->
      None

let ordered_list_item s =
  let rec loop n m s =
    match Sub.head s with
    | Some ('0'..'9' as c) ->
        if n >= 9 then None
        else loop (succ n) (m * 10 + Char.code c - Char.code '0') (Sub.tail s)
    | Some ('.' | ')' as c) ->
        let s = Sub.tail s in
        if is_empty s then
          Some (Ordered (m, c), n + 1, s)
        else
          let ind = indent s in
          if ind = 0 then None
          else begin
            let ind = if ind <= 4 then ind else 1 in
            Some (Ordered (m, c), n + ind, Sub.offset ind s)
          end
    | Some _ | None ->
        None
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
      Some (loop 1 (Sub.tail s0))
  | Some _ | None ->
      None

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

let known_tag s =
  List.mem (String.lowercase_ascii s) known_tags

(* let html_closing tag s = *)
(*   let has_ws = *)
(*     match Sub.head s with Some (' ' | '\t' | '\010'..'\013') | None -> true | Some _ -> false *)
(*   in *)
(*   let s = ws s in *)
(*   match Sub.head s with *)
(*   | Some '>' -> *)
(*       if is_empty (Sub.tail s) then *)
(*         Some (false, Hblank) *)
(*       else if known_tag tag then *)
(*         Some (true, Hblank) *)
(*       else *)
(*         None *)
(*   | Some _ | None -> *)
(*       if has_ws && known_tag tag then *)
(*         Some (true, Hblank) *)
(*       else *)
(*         None *)

let attribute s =
  let s = ws1 s in
  match Sub.head s with

let html_open tag s =
  match Sub.heads 2 s with
  | ([] | (' ' | '\t' | '\010'..'\013') :: _ | '/' :: '>' :: _ | '>' :: _), _ ->
      if known_tag tag then Some (true, Hblank) else None
  | _ ->
      let al, s = list attribute s in
      let s = ws s in
      begin match Sub.heads 2 s with
      | ('/' :: '>' :: _ | '>' :: _), s ->
          if is_blank s then
            Some (a = [] && known_tag tag, Hblank)
          else
            None
      | _ ->
          None
      end

let raw_html s =
  match Sub.heads 10 s with
  | '<' :: '?' :: _, _ ->
      Some (true, Hcontains ["?>"])
  | '<' :: '!' :: '-' :: '-' :: _, _ ->
      Some (true, Hcontains ["-->"])
  | '<' :: '!' :: '[' :: 'C' :: 'D' :: 'A' :: 'T' :: 'A' :: '[' :: _, _ ->
      Some (true, Hcontains ["]]>"])
  | '<' :: '!' :: _, _ ->
      Some (true, Hcontains [">"])
  | '<' :: '/' :: _, s ->
      begin match tag_name (Sub.tail s) with
      | Some (tag, s) ->
          html_closing tag s
      | None ->
          None
      end
  | '<' :: _, s ->
      begin match tag_name s with
      | Some (tag, s) ->
          html_open tag s
      | None ->
          None
      end
  | _ ->
      None

let parse s0 =
  let ind, s = sp3 s0 in
  match Sub.head s with
  | Some '>' ->
      let n = ind + 1 in
      let s = Sub.offset n s in
      let s = if indent s > 0 then Sub.offset 1 s else s in
      Lblockquote s
  | Some '=' ->
      begin match setext_heading s with
      | Some m ->
          Lsetext_heading (1, m)
      | None ->
          Lparagraph
      end
  | Some ('-' as c) ->
      begin match setext_heading s with
      | Some m ->
          Lsetext_heading (2, m)
      | None ->
          if thematic_break c s then
            Lthematic_break
          else begin
            match unordered_list_item s with
            | Some (k, n, s) ->
                Llist_item (k, n + ind, s)
            | None ->
                Lparagraph
          end
      end
  | Some ('_' as c) ->
      if thematic_break c s then Lthematic_break else Lparagraph
  | Some '#' ->
      begin match atx_heading s with
      | Some (n, s) ->
          Latx_heading (n, Sub.to_string s)
      | None ->
          Lparagraph
      end
  | Some ('~' | '`' as c) ->
      begin match code_block s with
      | Some (n, s) ->
          let c = if c = '~' then Tilde else Backtick in
          Lfenced_code (ind, n, c, Sub.to_string s)
      | None ->
          Lparagraph
      end
  | Some '<' ->
      begin match raw_html s with
      | Some (p, s) ->
          Lhtml (p, s)
      | None ->
          Lparagraph
      end
  | Some ('*' as c) ->
      if thematic_break c s then
        Lthematic_break
      else begin
        match unordered_list_item s with
        | Some (k, n, s) ->
            Llist_item (k, n + ind, s)
        | None ->
            Lparagraph
      end
  | Some '+' ->
      begin match unordered_list_item s  with
      | Some (k, n, s) ->
          Llist_item (k, n + ind, s)
      | None ->
          Lparagraph
      end
  | Some ('0'..'9') ->
      begin match ordered_list_item s with
      | Some (k, n, s) ->
          Llist_item (k, n + ind, s)
      | None ->
          Lparagraph
      end
  | Some _ ->
      if is_empty s then
        Lempty
      else if indent s0 >= 4 then
        Lindented_code (Sub.offset 4 s0)
      else
        Lparagraph
  | None ->
      Lempty

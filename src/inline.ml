(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013-2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Ast

type delim =
  | Ws
  | Punct
  | Other

type emph =
  | Star
  | Underscore

type t =
  | Bang_left_bracket
  | Left_bracket
  | Emph of delim * delim * emph * int
  | R of Ast.inline

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

let to_r : _ -> Ast.inline = function
  | Bang_left_bracket -> Text "!["
  | Left_bracket -> Text "["
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
              if n1 >= 2 && n2 >= 2 then
                R (Bold (Ast.cat (parse_emph (List.rev acc)))) :: xs
              else
                R (Emph (Ast.cat (parse_emph (List.rev acc)))) :: xs
            in
            let r =
              if n1 >= 2 && n2 >= 2 then
                if n1 > 2 then Emph (pre, Punct, q1, n1-2) :: r else r
              else
                if n1 > 1 then Emph (pre, Punct, q1, n1-1) :: r else r
            in
            parse_emph r
        | x :: xs ->
            loop (x :: acc) xs
        | [] ->
            to_r x :: List.rev_map to_r acc
      in
      loop [] xs
  | x :: xs ->
      to_r x :: parse_emph xs
  | [] ->
      []

let percent_encode s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '!' | '*' | '\'' | '(' | ')' | ';' | ':'
      | '@' | '&' | '=' | '+' | '$' | ',' | '/' | '?'
      | '#' | '[' | ']'
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' as c ->
          Buffer.add_char b c
      | _ as c ->
          Printf.bprintf b "%%%2X" (Char.code c)
    ) s;
  Buffer.contents b

let rec html_of_md b md =
  let rec loop = function
    | Cat l ->
        List.iter loop l
    | Img (alt, src, title) ->
        Buffer.add_string b "<img src=\"";
        Buffer.add_string b (Utils.htmlentities ~md:true src);
        Buffer.add_string b "\" alt=\"";
        Buffer.add_string b (Utils.htmlentities ~md:true alt);
        Buffer.add_string b "\" ";
        if title <> "" then begin
          Buffer.add_string b " title='";
          Buffer.add_string b (Utils.htmlentities ~md:true title);
          Buffer.add_string b "' "
        end;
        Buffer.add_string b "/>"
    | Text t ->
        (* Buffer.add_string b t; *)
        Buffer.add_string b (Utils.htmlentities ~md:true t)
    | Emph md ->
        Buffer.add_string b "<em>";
        loop md;
        Buffer.add_string b "</em>"
    | Bold md ->
        Buffer.add_string b "<strong>";
        loop md;
        Buffer.add_string b "</strong>"
    | Code c ->
        Buffer.add_string b "<code>";
        Buffer.add_string b (Utils.htmlentities ~md:false c);
        Buffer.add_string b "</code>"
    | Hard_break ->
        Buffer.add_string b "<br />\n"
    | Html body ->
        Buffer.add_string b body
    | Url (s, href, title) ->
        Buffer.add_string b "<a href=\"";
        Buffer.add_string b (percent_encode href);
        Buffer.add_string b "\"";
        begin match title with
        | None -> ()
        | Some title ->
            Buffer.add_string b " title=\"";
            Buffer.add_string b (Utils.htmlentities ~md:true title);
            Buffer.add_string b "\""
        end;
        Buffer.add_string b ">";
        html_of_md b s;
        Buffer.add_string b "</a>"
    | Soft_break ->
        Buffer.add_string b "\n"
  in
  loop md

let escape_markdown_characters s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '.' as c ->
        if i > 0 &&
           match s.[i-1] with
           | '0' .. '9' -> i+1 < String.length s && s.[i+1] = ' '
           | _ -> false
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '-' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && (s.[i+1] = ' '||s.[i+1] = '-'))
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '+' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && s.[i+1] = ' ')
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '!' as c ->
        if i+1 < String.length s && s.[i+1] = '[' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '<' as c ->
        if i <> String.length s - 1 &&
           (match s.[i+1] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true)
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '>' as c ->
        if i = 0 || (match s.[i-1] with ' ' | '\n' -> false | _ -> true) then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '#' as c ->
        if i = 0 || s.[i-1] = '\n' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '\\' | '[' | ']' | '(' | ')' | '`' | '*' as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let rec markdown_of_md md =
  let b = Buffer.create 64 in
  let rec loop = function
    | Cat l ->
        List.iter loop l
    | Img (alt, src, title) ->
        Printf.bprintf b "![%s](%s \"%s\")" alt src title
    | Text t ->
        Printf.bprintf b "%s" (escape_markdown_characters t)
    | Emph md ->
        Buffer.add_string b "*";
        loop md;
        Buffer.add_string b "*"
    | Bold md ->
        Buffer.add_string b "**";
        loop md;
        Buffer.add_string b "**"
    | Code c ->
        let n = (* compute how many backquotes we need to use *)
          let filter (n:int) (s:int list) =
            if n > 0 && n < 10 then
              List.filter (fun e -> e <> n) s
            else
              s
          in
          let l = String.length c in
          let rec loop s x b i =
            if i = l then begin
              match filter b s with hd :: _ -> hd | [] -> x+1
            end else begin
              match c.[i] with
              | '`' ->
                  loop s x (succ b) (succ i)
              | _ ->
                  loop (filter b s) (max b x) 0 (succ i)
            end
          in
          loop [1;2;3;4;5;6;7;8;9;10] 0 0 0
        in
        Printf.bprintf b "%s" (String.make n '`');
        if c.[0] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" c;
        if c.[String.length c - 1] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" (String.make n '`')
    | Hard_break ->
        Buffer.add_string b "<br />"
    | Html body ->
        Buffer.add_string b body
    | Url (s, href, None) ->
        Printf.bprintf b "[%s](%s)" (markdown_of_md s) href
    | Url (s, href, Some title) ->
        Printf.bprintf b "[%s](%s \"%s\")" (markdown_of_md s) href title
    | Soft_break ->
        if Buffer.length b = 1 ||
           (Buffer.length b > 1 &&
            not(Buffer.nth b (Buffer.length b - 1) = '\n' &&
                Buffer.nth b (Buffer.length b - 2) = '\n'))
        then
          Buffer.add_string b "\n"
  in
  loop md;
  let res = Buffer.contents b in
  res

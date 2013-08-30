(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(* Implementation notes *********************************************

   * - This module should depend on OCaml's standard library only and
   * should be as 'pure OCaml' (i.e. depend as least as possible on
   * external tools) as possible.

   * - `while' loops are sometimes preferred to recursion because this
   * may be used on systems where tail recursion is not well
   * supported. (I tried to write "while" as often as possible, but it
   * turned out that it was pretty inconvenient, so I do use
   * recursion.  When I have time, I'll do some tests and see if I
   * need to convert recursive loops into iterative loops. Sorry if it
   * makes it harder to read.)

*)

(* class type tag = object method is_me : 'a. 'a -> bool end *)

open Omd_representation

let string_of_t = function
  | Tag _ -> ""
  | Ampersand -> "&"
  | Ampersands n -> String.make (2+n) '&'
  | At -> "@"
  | Ats  n -> String.make (2+n) '@'
  | Backquote -> "`"
  | Backquotes  n -> String.make (2+n) '`'
  | Backslash -> "\\"
  | Backslashs  n -> String.make (2+n) '\\'
  | Bar -> "|"
  | Bars  n -> String.make (2+n) '|'
  | Caret -> "^"
  | Carets  n -> String.make (2+n) '^'
  | Cbrace -> "}"
  | Cbraces  n -> String.make (2+n) '}'
  | Colon -> ":"
  | Colons  n -> String.make (2+n) ':'
  | Comma -> ","
  | Commas  n -> String.make (2+n) ','
  | Cparenthesis -> ")"
  | Cparenthesiss  n -> String.make (2+n) ')'
  | Cbracket -> "]"
  | Cbrackets  n -> String.make (2+n) ']'
  | Dollar -> "$"
  | Dollars  n -> String.make (2+n) '$'
  | Dot -> "."
  | Dots  n -> String.make (2+n) '.'
  | Doublequote -> "\""
  | Doublequotes  n -> String.make (2+n) '"'
  | Exclamation -> "!"
  | Exclamations  n -> String.make (2+n) '!'
  | Equal -> "="
  | Equals  n -> String.make (2+n) '='
  | Greaterthan -> ">"
  | Greaterthans  n -> String.make (2+n) '>'
  | Hash -> "#"
  | Hashs  n -> String.make (2+n) '#'
  | Lessthan -> "<"
  | Lessthans  n -> String.make (2+n) '<'
  | Minus -> "-"
  | Minuss  n -> String.make (2+n) '-'
  | Newline -> "\n"
  | Newlines  n -> String.make (2+n) '\n'
  | Number s -> s
  | Obrace -> "{"
  | Obraces  n -> String.make (2+n) '{'
  | Oparenthesis -> "("
  | Oparenthesiss  n -> String.make (2+n) '('
  | Obracket -> "["
  | Obrackets  n -> String.make (2+n) '['
  | Percent -> "%"
  | Percents  n -> String.make (2+n) '%'
  | Plus -> "+"
  | Pluss  n -> String.make (2+n) '+'
  | Question -> "?"
  | Questions  n -> String.make (2+n) '?'
  | Quote -> "'"
  | Quotes  n -> String.make (2+n) '\''
  | Semicolon -> ";"
  | Semicolons  n -> String.make (2+n) ';'
  | Slash -> "/"
  | Slashs  n -> String.make (2+n) '/'
  | Space -> " "
  | Spaces  n -> String.make (2+n) ' '
  | Star -> "*"
  | Stars  n -> String.make (2+n) '*'
  | Tab -> "\t"
  | Tabs  n -> String.make (2+n) '\t'
  | Tilde -> "~"
  | Tildes  n -> String.make (2+n) '~'
  | Underscore -> "_"
  | Underscores  n -> String.make (2+n) '_'
  | Word s -> s



let lex s =
  let result = ref [] in
  let i = ref 0 in
  let l = String.length s in
  let rcount c =
    (* [rcount c] returns the number of immediate consecutive
       occurrences of [c].  By side-effect, it increases the reference
       counter [i]. *)
    let rec loop r =
      if !i = l then r
      else if s.[!i] = c then (incr i; loop (r+1))
      else r
    in
    loop 1
  in
  let word () =
    let start = !i in
    let rec loop () =
      begin
        if !i = l then
          Word (String.sub s start (!i-start))
        else
          match s.[!i] with
          (* FIXME: pattern matching on chars is inefficient *)
          | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\''
          | '"' | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':'
          | ';' | '>' | '~' | '<' | '@' | '&' | '|' | '^' | '.' | '/'
          | '$' | '%' | '!' | '?' ->
                               Word (String.sub s start (!i-start))
          | c -> incr i; loop()
      end
    in
    loop()
  in
  let maybe_number () =
    let start = !i in
    while
      match s.[!i] with
      | '0' .. '9' -> true
      | _ -> false
    do
      incr i
    done;
    match s.[!i] with
    (* FIXME: pattern matching on chars is inefficient *)
    | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\'' | '"'
    | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>'
    | '~' | '<' | '@' | '&' | '|' | '^' | '.' | '/' | '$' | '%' | '!'
    | '?' ->
       Number(String.sub s start (!i-start))
    | _ ->
       i := start;
       word()
  in

  let n_occ c = incr i; rcount c in

  while !i < l do
    let c = s.[!i] in
    let w = match c with
      (* FIXME: pattern matching on chars is inefficient *)
      | ' '  -> let n = n_occ c in if n = 1 then Space else Spaces (n-2)
      | '\t' -> let n = n_occ c in if n = 1 then Tab else Tabs (n-2)
      | '\n' -> let n = n_occ c in if n = 1 then Newline else Newlines (n-2)
      | '\r' -> (* eliminating \r by converting all styles to unix style *)
          let n = n_occ c in
            if n = 1 then
              if !i = l then
                Newline
              else
                if s.[!i] = '\n' then
                  (incr i; Newline)
                else
                  Newline
            else 
              Newlines (n-2) 
      | '#'  -> let n = n_occ c in if n = 1 then Hash else Hashs (n-2)
      | '*'  -> let n = n_occ c in if n = 1 then Star else Stars (n-2)
      | '-'  -> let n = n_occ c in if n = 1 then Minus else Minuss (n-2)
      | '+'  -> let n = n_occ c in if n = 1 then Plus else Pluss (n-2)
      | '`'  -> let n = n_occ c in if n = 1 then Backquote else Backquotes (n-2)
      | '\'' -> let n = n_occ c in if n = 1 then Quote else Quotes (n-2)
      | '"'  -> let n = n_occ c in if n = 1 then Doublequote
                                  else Doublequotes (n-2)
      | '\\' -> let n = n_occ c in if n = 1 then Backslash
                                  else Backslashs (n-2)
      | '_'  -> let n = n_occ c in if n = 1 then Underscore
                                  else Underscores (n-2)
      | '['  -> let n = n_occ c in if n = 1 then Obracket
                                  else Obrackets (n-2)
      | ']'  -> let n = n_occ c in if n = 1 then Cbracket else Cbrackets (n-2)
      | '{'  -> let n = n_occ c in if n = 1 then Obrace else Obraces (n-2)
      | '}'  -> let n = n_occ c in if n = 1 then Cbrace else Cbraces (n-2)
      | '('  -> let n = n_occ c in if n = 1 then Oparenthesis
                                  else Oparenthesiss (n-2)
      | ')'  -> let n = n_occ c in if n = 1 then Cparenthesis
                                  else Cparenthesiss (n-2)
      | ':'  -> let n = n_occ c in if n = 1 then Colon else Colons (n-2)
      | ';'  -> let n = n_occ c in if n = 1 then Semicolon else Semicolons (n-2)
      | '>'  -> let n = n_occ c in if n = 1 then Greaterthan
                                  else Greaterthans (n-2)
      | '~'  -> let n = n_occ c in if n = 1 then Tilde else Tildes (n-2)
      | '<'  -> let n = n_occ c in if n = 1 then Lessthan else Lessthans (n-2)
      | '@'  -> let n = n_occ c in if n = 1 then At else Ats (n-2)
      | '&'  -> let n = n_occ c in if n = 1 then Ampersand else Ampersands (n-2)
      | '|'  -> let n = n_occ c in if n = 1 then Bar else Bars (n-2)
      | '^'  -> let n = n_occ c in if n = 1 then Caret else Carets (n-2)
      | ','  -> let n = n_occ c in if n = 1 then Comma else Commas (n-2)
      | '.'  -> let n = n_occ c in if n = 1 then Dot else Dots (n-2)
      | '/'  -> let n = n_occ c in if n = 1 then Slash else Slashs (n-2)
      | '$'  -> let n = n_occ c in if n = 1 then Dollar else Dollars (n-2)
      | '%'  -> let n = n_occ c in if n = 1 then Percent else Percents (n-2)
      | '='  -> let n = n_occ c in if n = 1 then Equal else Equals (n-2)
      | '!'  -> let n = n_occ c in if n = 1 then Exclamation
                                  else Exclamations (n-2)
      | '?'  -> let n = n_occ c in if n = 1 then Question else Questions (n-2)
      | '0' .. '9' -> maybe_number()
      | c -> word() in
    result := w :: !result
  done;
  List.rev !result

let size = function
  | Tag _ -> (0, 0)
  | Ampersand | At | Backquote | Backslash | Bar | Caret | Cbrace
  | Colon | Comma | Cparenthesis | Cbracket | Dollar | Dot
  | Doublequote | Exclamation | Equal | Greaterthan | Hash | Lessthan
  | Minus | Obrace | Oparenthesis | Obracket | Percent | Plus
  | Question | Quote | Semicolon | Slash | Space | Star | Tab
  | Tilde | Underscore -> (1, 0)
  | Ampersands x | Ats x | Backquotes x | Backslashs x | Bars x | Carets x
  | Cbraces x | Colons x | Commas x | Cparenthesiss x | Cbrackets x
  | Dollars x | Dots x
  | Doublequotes x | Exclamations x | Equals x | Greaterthans x | Hashs x
  | Lessthans x
  | Minuss x | Obraces x | Oparenthesiss x | Obrackets x | Percents x | Pluss x
  | Questions x | Quotes x | Semicolons x | Slashs x | Spaces x | Stars x
  | Tabs x
  | Tildes x | Underscores x -> (2+x, 0)
  | Newline -> (0, 1)
  | Newlines x -> (0, 2+x)
  | Number s | Word s -> (String.length s, 0)

let make_space = function
  | 0 -> raise (Invalid_argument "Md_lexer.make_space")
  | 1 -> Space
  | n -> if n < 0 then raise (Invalid_argument "Md_lexer.make_space") else
        Spaces (n-2)

let position orig spot =
  let ( ++ ) (x,y) (a,b) =
    if b = 0 then
      (x+a, y)
    else
      (a, y+b)
  in
  let rec loop r = function
    | (hd :: tl) as l ->
        if l == spot then
          r
        else
          loop (r ++ size hd) tl
    | [] -> r
  in
    loop (0,0)

let _ =
  lex "42 Bonjour !!\n"
;;

(*
(** [string_of_tl l] returns the string representation of l.
    [estring_of_tl l] returns the escaped string representation of l
    (same semantics as [String.escaped (string_of_tl l)]). *)
let string_of_tl, estring_of_tl =
  let g escaped tl =
    let b = Buffer.create 42 in
    let rec loop : 'a t list -> unit = function
      | e::tl ->
          Buffer.add_string b (if escaped then String.escaped (string_of_t e)
                               else string_of_t e);
          loop tl
      | [] ->
          ()
    in
      Buffer.contents (loop tl; b)
  in g false, g true
*)

let string_of_tl tl =
  let b = Buffer.create 42 in
  let rec loop : tok list -> unit = function
    | e::tl ->
        Buffer.add_string b (string_of_t e);
        loop tl
    | [] ->
        ()
  in
    Buffer.contents (loop tl; b)


let destring_of_tl tl =
  let b = Buffer.create 42 in
  let rec loop : tok list -> unit = function
    | e::tl ->
        Buffer.add_string b (String.escaped (string_of_t e));
        Buffer.add_string b "::";
        loop tl
    | [] ->
        Buffer.add_string b "[]"
  in
    Buffer.contents (loop tl; b)

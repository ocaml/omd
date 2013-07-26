(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
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

type t = (* "of int":  *)
  | Ampersand
  | Ampersands of int
  | At
  | Ats of int
  | Backquote
  | Backquotes of int
  | Backslash
  | Backslashs of int
  | Bar
  | Bars of int
  | Caret
  | Carets of int
  | Cbrace
  | Cbraces of int
  | Colon
  | Colons of int
  | Comma
  | Commas of int
  | Cparenthesis
  | Cparenthesiss of int
  | Cbracket
  | Cbrackets of int
  | Dollar
  | Dollars of int
  | Dot
  | Dots of int
  | Doublequote
  | Doublequotes of int
  | Exclamation
  | Exclamations of int
  | Equal
  | Equals of int
  | Greaterthan
  | Greaterthans of int
  | Hash
  | Hashs of int
  | Lessthan
  | Lessthans of int
  | Minus
  | Minuss of int
  | Newline
  | Newlines of int
  | Number of string
  | Obrace
  | Obraces of int
  | Oparenthesis
  | Oparenthesiss of int
  | Obracket
  | Obrackets of int
  | Percent
  | Percents of int
  | Plus
  | Pluss of int
  | Question
  | Questions of int
  | Quote
  | Quotes of int
  | Return
  | Returns of int
  | Semicolon
  | Semicolons of int
  | Slash
  | Slashs of int
  | Space
  | Spaces of int
  | Star
  | Stars of int
  | Tab
  | Tabs of int
  | Tilde
  | Tildes of int
  | Underscore
  | Underscores of int
  | Word of string


let string_of_t = function
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
  | Cbrace -> "{"
  | Cbraces  n -> String.make (2+n) '{'
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
  | Return -> "\r"
  | Returns  n -> String.make (2+n) '\r'
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



let lex_from_string s =
  let result = ref [] in
  let i = ref 0 in
  let l = String.length s in
  let rcount c =
    (* [rcount c] returns the number of immediate consecutive
       occurrences of [c].  By side-effect, it increases the reference
       counter [i]. *)
    let rec loop r =
      if !i = l then
        r
      else
        if s.[!i] = c then
          (incr i; loop (r+1))
        else
          r
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
            | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\'' | '"' 
            | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>' | '~'
            | '<' | '@' | '&' | '|' | '^' | '.' | '/' | '$' | '%' | '!' | '?' -> 
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
        | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\'' | '"' 
        | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>'  | '~'
        | '<' | '@' | '&' | '|' | '^' | '.' | '/' | '$' | '%' | '!' | '?' ->
            Number(String.sub s start (!i-start))
        | _ ->
            i := start;
            word()
  in
  let (++) _ w =
    result := w :: !result
  in

    while !i < l do
      () ++
        match s.[!i] with 
          | ' '  as c  -> incr i; (match (rcount c) with 1 -> Space | n -> Spaces (n-2))
          | '\t' as c  -> incr i; (match (rcount c) with 1 -> Tab | n -> Tabs (n-2))
          | '\n' as c  -> incr i; (match (rcount c) with 1 -> Newline | n -> Newlines (n-2))
          | '\r' as c  -> incr i; (match (rcount c) with 1 -> Return | n -> Returns (n-2))
          | '#'  as c  -> incr i; (match (rcount c) with 1 -> Hash | n -> Hashs (n-2))
          | '*'  as c  -> incr i; (match (rcount c) with 1 -> Star | n -> Stars (n-2))
          | '-'  as c  -> incr i; (match (rcount c) with 1 -> Minus | n -> Minuss (n-2))
          | '+'  as c  -> incr i; (match (rcount c) with 1 -> Plus | n -> Pluss (n-2))
          | '`'  as c  -> incr i; (match (rcount c) with 1 -> Backquote | n -> Backquotes (n-2))
          | '\'' as c  -> incr i; (match (rcount c) with 1 -> Quote | n -> Quotes (n-2))
          | '"'  as c  -> incr i; (match (rcount c) with 1 -> Doublequote | n -> Doublequotes (n-2))
          | '\\' as c  -> incr i; (match (rcount c) with 1 -> Backslash | n -> Backslashs (n-2))
          | '_'  as c  -> incr i; (match (rcount c) with 1 -> Underscore | n -> Underscores (n-2))
          | '['  as c  -> incr i; (match (rcount c) with 1 -> Obracket | n -> Obrackets (n-2))
          | ']'  as c  -> incr i; (match (rcount c) with 1 -> Cbracket | n -> Cbrackets (n-2))
          | '{'  as c  -> incr i; (match (rcount c) with 1 -> Obrace | n -> Obraces (n-2))
          | '}'  as c  -> incr i; (match (rcount c) with 1 -> Cbrace | n -> Cbraces (n-2))
          | '('  as c  -> incr i; (match (rcount c) with 1 -> Oparenthesis | n -> Oparenthesiss (n-2))
          | ')'  as c  -> incr i; (match (rcount c) with 1 -> Cparenthesis | n -> Cparenthesiss (n-2))
          | ':'  as c  -> incr i; (match (rcount c) with 1 -> Colon | n -> Colons (n-2))
          | ';'  as c  -> incr i; (match (rcount c) with 1 -> Semicolon | n -> Semicolons (n-2))
          | '>'  as c  -> incr i; (match (rcount c) with 1 -> Greaterthan | n -> Greaterthans (n-2))
          | '~'  as c  -> incr i; (match (rcount c) with 1 -> Tilde | n -> Tildes (n-2))
          | '<'  as c  -> incr i; (match (rcount c) with 1 -> Lessthan | n -> Lessthans (n-2))
          | '@'  as c  -> incr i; (match (rcount c) with 1 -> At | n -> Ats (n-2))
          | '&'  as c  -> incr i; (match (rcount c) with 1 -> Ampersand | n -> Ampersands (n-2))
          | '|'  as c  -> incr i; (match (rcount c) with 1 -> Bar | n -> Bars (n-2))
          | '^'  as c  -> incr i; (match (rcount c) with 1 -> Caret | n -> Carets (n-2))
          | ','  as c  -> incr i; (match (rcount c) with 1 -> Comma | n -> Comma (n-2))
          | '.'  as c  -> incr i; (match (rcount c) with 1 -> Dot | n -> Dots (n-2))
          | '/'  as c  -> incr i; (match (rcount c) with 1 -> Slash | n -> Slashs (n-2))
          | '$'  as c  -> incr i; (match (rcount c) with 1 -> Dollar | n -> Dollars (n-2))
          | '%'  as c  -> incr i; (match (rcount c) with 1 -> Percent | n -> Percents (n-2))
          | '='  as c  -> incr i; (match (rcount c) with 1 -> Equal | n -> Equals (n-2))
          | '!'  as c  -> incr i; (match (rcount c) with 1 -> Exclamation | n -> Exclamations (n-2))
          | '?'  as c  -> incr i; (match (rcount c) with 1 -> Question | n -> Questions (n-2))
          | '0' .. '9' -> maybe_number()
          | c -> word()
    done;
    List.rev !result

let rec convert_to_lf = function
  | [] -> []
  | Return :: Newline :: tl -> Newline :: convert_to_lf tl
  | Return :: tl -> Newline :: convert_to_lf tl
  | Returns n :: tl -> Newlines n :: convert_to_lf tl
  | hd :: tl -> hd :: convert_to_lf tl

let rec convert_crlf_to_lf = function
  | [] -> []
  | Return :: Newline :: tl -> Newline :: convert_crlf_to_lf tl
  | hd :: tl -> hd :: convert_crlf_to_lf tl

let rec convert_cr_to_lf = function
  | [] -> []
  | Return :: tl -> Newline :: convert_cr_to_lf tl
  | Returns n :: tl -> Newlines n :: convert_cr_to_lf tl
  | hd :: tl -> hd :: convert_cr_to_lf tl

let rec convert_to_crlf = function
  | [] -> []
  | Return :: Newline :: tl -> Return :: Newline :: convert_to_crlf tl
  | Newline :: tl -> Return :: Newline :: convert_to_crlf tl
  | Newlines 0 :: tl -> Return :: Newline :: Return :: Newline :: convert_to_crlf tl
  | Newlines n :: tl -> Return :: Newline :: convert_to_crlf (Newlines (n-1) :: tl)
  | Returns 0 :: tl -> Return :: Newline :: Return :: Newline :: convert_to_crlf tl
  | Returns n :: tl -> Return :: Newline :: convert_to_crlf (Returns (n-1) :: tl)
  | hd :: tl -> hd :: convert_to_crlf tl


let length = function
  | Ampersand | At | Backquote | Backslash | Bar | Caret | Cbrace
  | Colon | Comma | Cparenthesis | Cbracket | Dollar | Dot
  | Doublequote | Exclamation | Equal | Greaterthan | Hash | Lessthan 
  | Minus | Obrace | Oparenthesis | Obracket | Percent | Plus
  | Question | Quote | Semicolon | Slash | Space | Star | Tab 
  | Tilde | Underscore -> (1, 0)
  | Ampersands x | Ats x | Backquotes x | Backslashs x | Bars x | Carets x
  | Cbraces x | Colons x | Comma x | Cparenthesiss x | Cbrackets x | Dollars x | Dots x
  | Doublequotes x | Exclamations x | Equals x | Greaterthans x | Hashs x | Lessthans x 
  | Minuss x | Obraces x | Oparenthesiss x | Obrackets x | Percents x | Pluss x
  | Questions x | Quotes x | Semicolons x | Slashs x | Spaces x | Stars x | Tabs x 
  | Tildes x | Underscores x -> (2+x, 0)
  | Return | Newline -> (0, 1)
  | Returns x | Newlines x -> (0, 2+x)
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
          loop (r ++ length hd) tl
    | [] -> r
  in
    loop (0,0)

let _ =
  lex_from_string "42 Bonjour !!\n"
;;
let _ =
  convert_to_lf (lex_from_string "42 Bonjour !\n")
;;

(** [string_of_tl l] returns the string representation of l.
    [estring_of_tl l] returns the escaped string representation of l (same semantics as [String.escaped (string_of_tl l)]). *)
let string_of_tl, estring_of_tl =
  let g escaped tl =
    let b = Buffer.create 42 in
    let rec loop : t list -> unit = function
      | e::tl ->
          Buffer.add_string b (if escaped then String.escaped (string_of_t e) else string_of_t e);
          loop tl
      | [] ->
          ()
    in 
      Buffer.contents (loop tl; b)
  in g false, g true


let dstring_of_tl, destring_of_tl =
  let g escaped tl =
    let b = Buffer.create 42 in
    let rec loop : t list -> unit = function
      | e::tl ->
          Buffer.add_string b (if escaped then String.escaped (string_of_t e) else string_of_t e);
          Buffer.add_string b "::";
          loop tl
      | [] ->
          Buffer.add_string b "[]"
    in 
      Buffer.contents (loop tl; b)
  in g false, g true

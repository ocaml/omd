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

type t =
  | Ampersand of int
  | At of int
  | Backquote of int
  | Backslash of int
  | Bar of int
  | Caret of int
  | Cbrace of int
  | Colon of int
  | Cparenthesis of int
  | Cbracket of int
  | Dollar of int
  | Dot of int
  | Doublequote of int
  | Exclamation of int
  | Equal of int
  | Greaterthan of int
  | Hash of int
  | Lessthan of int
  | Minus of int
  | Newline of int
  | Number of string
  | Obrace of int
  | Oparenthesis of int
  | Obracket of int
  | Percent of int
  | Plus of int
  | Question of int
  | Quote of int
  | Return of int
  | Semicolon of int
  | Slash of int
  | Space of int
  | Star of int
  | Tab of int
  | Tilde of int
  | Underscore of int
  | Word of string


let string_of_t = function
  | Ampersand n -> String.make n '&'
  | At  n -> String.make n '@'
  | Backquote  n -> String.make n '`'
  | Backslash  n -> String.make n '\\'
  | Bar  n -> String.make n '|'
  | Caret  n -> String.make n '^'
  | Cbrace  n -> String.make n '{'
  | Colon  n -> String.make n ','
  | Cparenthesis  n -> String.make n ')'
  | Csbracket  n -> String.make n ']'
  | Dollar  n -> String.make n '$'
  | Dot  n -> String.make n '.'
  | Doublequote  n -> String.make n '"'
  | Exclamation  n -> String.make n '!'
  | Equal  n -> String.make n '='
  | Greaterthan  n -> String.make n '>'
  | Hash  n -> String.make n '#'
  | Lessthan  n -> String.make n '<'
  | Minus  n -> String.make n '-'
  | Newline  n -> String.make n '\n'
  | Number s -> s
  | Obrace  n -> String.make n '{'
  | Oparenthesis  n -> String.make n '('
  | Osbracket  n -> String.make n '['
  | Percent  n -> String.make n '%'
  | Plus  n -> String.make n '+'
  | Question  n -> String.make n '?'
  | Quote  n -> String.make n '\''
  | Return  n -> String.make n '\r'
  | Semicolon  n -> String.make n ';'
  | Slash  n -> String.make n '/'
  | Space  n -> String.make n ' '
  | Star  n -> String.make n '*'
  | Tab  n -> String.make n '\t'
  | Tilde  n -> String.make n '~'
  | Underscore  n -> String.make n '_'
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
          | ' '  as c  -> incr i; Space (rcount c)
          | '\t' as c  -> incr i; Tab (rcount c)
          | '\n' as c  -> incr i; Newline (rcount c)
          | '\r' as c  -> incr i; Return (rcount c)
          | '#'  as c  -> incr i; Hash (rcount c)
          | '*'  as c  -> incr i; Star (rcount c)
          | '-'  as c  -> incr i; Minus (rcount c)
          | '+'  as c  -> incr i; Plus (rcount c)
          | '`'  as c  -> incr i; Backquote (rcount c)
          | '\'' as c  -> incr i; Quote (rcount c)
          | '"'  as c  -> incr i; Doublequote (rcount c)
          | '\\' as c  -> incr i; Backslash (rcount c)
          | '_'  as c  -> incr i; Underscore (rcount c)
          | '['  as c  -> incr i; Osbracket (rcount c)
          | ']'  as c  -> incr i; Csbracket (rcount c)
          | '{'  as c  -> incr i; Obrace (rcount c)
          | '}'  as c  -> incr i; Cbrace (rcount c)
          | '('  as c  -> incr i; Oparenthesis (rcount c)
          | ')'  as c  -> incr i; Cparenthesis (rcount c)
          | ':'  as c  -> incr i; Colon (rcount c)
          | ';'  as c  -> incr i; Semicolon (rcount c)
          | '>'  as c  -> incr i; Greaterthan (rcount c)
          | '~'  as c  -> incr i; Tilde (rcount c)
          | '<'  as c  -> incr i; Lessthan (rcount c)
          | '@'  as c  -> incr i; At (rcount c)
          | '&'  as c  -> incr i; Ampersand (rcount c)
          | '|'  as c  -> incr i; Bar (rcount c)
          | '^'  as c  -> incr i; Caret (rcount c)
          | '.'  as c  -> incr i; Dot (rcount c)
          | '/'  as c  -> incr i; Slash (rcount c)
          | '$'  as c  -> incr i; Dollar (rcount c)
          | '%'  as c  -> incr i; Percent (rcount c)
          | '='  as c  -> incr i; Equal (rcount c)
          | '!'  as c  -> incr i; Exclamation (rcount c)
          | '?'  as c  -> incr i; Question (rcount c)
          | '0' .. '9' -> maybe_number()
          | c -> word()
    done;
    List.rev !result

let rec convert_to_lf = function
  | [] -> []
  | Return 1 :: Newline 1 :: tl -> Newline 1 :: convert_to_lf tl
  | Return n :: tl -> Newline n :: convert_to_lf tl
  | hd :: tl -> hd :: convert_to_lf tl

let rec convert_crlf_to_lf = function
  | [] -> []
  | Return 1 :: Newline 1 :: tl -> Newline 1 :: convert_crlf_to_lf tl
  | hd :: tl -> hd :: convert_crlf_to_lf tl

let rec convert_cr_to_lf = function
  | [] -> []
  | Return n :: tl -> Newline n :: convert_cr_to_lf tl
  | hd :: tl -> hd :: convert_cr_to_lf tl

let rec convert_to_crlf = function
  | [] -> []
  | Return 1 :: Newline 1 :: tl -> Return 1 :: Newline 1 :: convert_to_crlf tl
  | Newline 1 :: tl -> Return 1 :: Newline 1 :: convert_to_crlf tl
  | Newline n :: tl -> assert (n>0);
      Return 1 :: Newline 1 :: convert_to_crlf (Newline (n-1) :: tl)
  | Return n :: tl -> assert (n>0);
      Return 1 :: Newline 1 :: convert_to_crlf (Return (n-1) :: tl)
  | hd :: tl -> hd :: convert_to_crlf tl


let position orig spot =
  let ( ++ ) (x,y) (a,b) =
    if b = 0 then
      (x+a, y)
    else
      (a, y+b)
  in 
  let length = function
    | Ampersand x | At x | Backquote x | Backslash x | Bar x | Caret x
    | Cbrace x | Colon x | Cparenthesis x | Csbracket x | Dollar x | Dot x
    | Doublequote x | Exclamation x | Equal x | Greaterthan x | Hash x | Lessthan x 
    | Minus x | Obrace x | Oparenthesis x | Osbracket x | Percent x | Plus x
    | Question x | Quote x | Semicolon x | Slash x | Space x | Star x | Tab x 
    | Tilde x | Underscore x -> (x, 0)
    | Newline x -> (0, x)
    | Return x -> (0, x)
    | Number s | Word s -> (String.length s, 0)
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
  lex_from_string "42 Bonjour !\n"
;;
let _ =
  convert_to_lf (lex_from_string "42 Bonjour !\n")
;;

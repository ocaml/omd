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
   * supported.

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
  | Csbracket of int
  | Dollar of int
  | Dot of int
  | Doublequote of int
  | Exclamation of int
  | Greaterthan of int
  | Hash of int
  | Lessthan of int
  | Minus of int
  | Newline of int
  | Number of string
  | Obrace of int
  | Oparenthesis of int
  | Osbracket of int
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
  | Underscore of int
  | Word of string

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
            | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>' 
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
        | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>' 
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
          | '<'  as c  -> incr i; Lessthan (rcount c)
          | '@'  as c  -> incr i; At (rcount c)
          | '&'  as c  -> incr i; Ampersand (rcount c)
          | '|'  as c  -> incr i; Bar (rcount c)
          | '^'  as c  -> incr i; Caret (rcount c)
          | '.'  as c  -> incr i; Dot (rcount c)
          | '/'  as c  -> incr i; Slash (rcount c)
          | '$'  as c  -> incr i; Dollar (rcount c)
          | '%'  as c  -> incr i; Percent (rcount c)
          | '!'  as c  -> incr i; Exclamation (rcount c)
          | '?'  as c  -> incr i; Question (rcount c)
          | '0' .. '9' -> maybe_number()
          | c -> word()
    done;
    List.rev !result


let _ =
  lex_from_string "42 Bonjour !\n"
;;

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



*)

type md_element = 
    | Text of string
    | Emph of md
    | Bold of md 
and md = md_element list
    

open Md_lexer


(** [emph_or_bold (n:int) (r:md list) (l: Md_lexer.t list)] 
    returns [] if not (emph and/or bold),
    else returns the contents intended to be formatted. *)
let rec emph_or_bold n r l =
  assert (n>0 && n<4);
  let rec loop (result:Md_lexer.t list) = function
    | [] -> []
    | Newline _ :: tl -> []
    | ((Star x) as t) :: tl ->
        if x = n then
          result
        else
          loop (t :: result) tl
    | _ -> assert false
  in List.rev (loop [] l)

let emph_or_bold n r l = failwith ""
let spaces r previous n l = failwith ""
let new_ulist r previous l = failwith ""

let parse lexemes =
  let rec loop (r: md) (previous:Md_lexer.t list) (lexemes:Md_lexer.t list) =
    match previous, lexemes with

      (* no more to process *)
      | _, [] ->
          r

      (* hashes *)
      | ([Newline _]|[]), Hash n :: tl -> (* hash titles *)
          read_title n tl
      | _, (Hash _ as t) :: tl -> (* hash -- no title *)
          loop
            (Text(string_of_t t) :: r)
            [t]
            tl

      (* spaces *)
      | _, Space (n) :: tl -> (* too many cases to be handled here *)
          spaces r previous n tl

      (* stars *)
      | ([]|[Newline _]), Star 1 :: tl -> (* one star at the beginning of a new line *)
          new_ulist r previous tl
      | _, (Star((1|2|3) as n) as t) :: tl -> (* 1, 2 or 3 "orphan" stars, or emph/bold *)
          begin match emph_or_bold n r tl with
            | [] -> loop (Text (string_of_t t) :: r) [t] tl
            | x  -> loop ((loop [] [t] x) @ r) [t] tl
          end
      | _, (Star n as t) :: tl -> (* one or several "orphan" stars, or emph/bold *)
          loop (Text(string_of_t t) :: r) [t] tl

      (* backslashes *)
      | _, Backslash 1 :: (Backquote 1 as t) :: tl -> (* \` *)
          loop (Text ("`") :: r) [t] tl
      | _, Backslash 1 :: Backquote n :: tl -> assert (n > 1); (* \````... *)
          loop (Text ("`") :: r) [Backslash 1; Backquote 1] (Backquote (n-1) :: tl)
      | _, Backslash 1 :: (Star 1 as t) :: tl -> (* \* *)
          loop (Text ("*") :: r) [t] tl
      | _, Backslash 1 :: Star n :: tl -> assert (n > 1); (* \****... *)
          loop (Text ("*") :: r) [Backslash 1; Star 1] (Star (n-1) :: tl)
      | _, Backslash 1 :: (Underscore 1 as t) :: tl -> (* \_ *)
          loop (Text ("_") :: r) [t] tl
      | _, Backslash 1 :: Underscore n :: tl -> assert (n > 1); (* \___... *)
          loop (Text ("_") :: r) [Backslash 1; Underscore 1] (Underscore (n-1) :: tl)
      | _, Backslash 1 :: (Obrace 1 as t) :: tl -> (* \{ *)
          loop (Text ("{") :: r) [t] tl
      | _, Backslash 1 :: Obrace n :: tl -> assert (n > 1); (* \{{{... *)
          loop (Text ("{") :: r) [Backslash 1; Obrace 1] (Obrace (n-1) :: tl)
      | _, Backslash 1 :: (Cbrace 1 as t) :: tl -> (* \} *)
          loop (Text ("}") :: r) [t] tl
      | _, Backslash 1 :: Cbrace n :: tl -> assert (n > 1); (* \}}}... *)
          loop (Text ("}") :: r) [Backslash 1; Cbrace 1] (Cbrace (n-1) :: tl)
      | _, Backslash 1 :: (Obracket 1 as t) :: tl -> (* \[ *)
          loop (Text ("[") :: r) [t] tl
      | _, Backslash 1 :: Obracket n :: tl -> assert (n > 1); (* \[[[... *)
          loop (Text ("[") :: r) [Backslash 1; Obracket 1] (Obracket (n-1) :: tl)
      | _, Backslash 1 :: (Cbracket 1 as t) :: tl -> (* \} *)
          loop (Text ("]") :: r) [t] tl
      | _, Backslash 1 :: Cbracket n :: tl -> assert (n > 1); (* \}}}... *)
          loop (Text ("]") :: r) [Backslash 1; Cbracket 1] (Cbracket (n-1) :: tl)
      | _, Backslash 1 :: (Oparenthesis 1 as t) :: tl -> (* \( *)
          loop (Text ("(") :: r) [t] tl
      | _, Backslash 1 :: Oparenthesis n :: tl -> assert (n > 1); (* \(((... *)
          loop (Text ("(") :: r) [Backslash 1; Oparenthesis 1] (Oparenthesis (n-1) :: tl)
      | _, Backslash 1 :: (Cparenthesis 1 as t) :: tl -> (* \) *)
          loop (Text (")") :: r) [t] tl
      | _, Backslash 1 :: Cparenthesis n :: tl -> assert (n > 1); (* \)))... *)
          loop (Text (")") :: r) [Backslash 1; Cparenthesis 1] (Cparenthesis (n-1) :: tl)
      | _, Backslash 1 :: (Plus 1 as t) :: tl -> (* \+ *)
          loop (Text ("+") :: r) [t] tl
      | _, Backslash 1 :: Plus n :: tl -> assert (n > 1); (* \+++... *)
          loop (Text ("+") :: r) [Backslash 1; Plus 1] (Plus (n-1) :: tl)
      | _, Backslash 1 :: (Minus 1 as t) :: tl -> (* \- *)
          loop (Text ("-") :: r) [t] tl
      | _, Backslash 1 :: Minus n :: tl -> assert (n > 1); (* \---... *)
          loop (Text ("-") :: r) [Backslash 1; Minus 1] (Minus (n-1) :: tl)
      | _, Backslash 1 :: (Dot 1 as t) :: tl -> (* \. *)
          loop (Text (".") :: r) [t] tl
      | _, Backslash 1 :: Dot n :: tl -> assert (n > 1); (* \....... *)
          loop (Text (".") :: r) [Backslash 1; Dot 1] (Dot (n-1) :: tl)
      | _, Backslash 1 :: (Exclamation 1 as t) :: tl -> (* \! *)
          loop (Text ("!") :: r) [t] tl
      | _, Backslash 1 :: Exclamation n :: tl -> assert (n > 1); (* \!!!... *)
          loop (Text ("!") :: r) [Backslash 1; Exclamation 1] (Exclamation (n-1) :: tl)

            
      | _, (Backslash n as t) :: tl -> (* \\\\... *)
          if n mod 2 = 0 then
            loop (Text (String.make (n/2) '\\') :: r) [t](*???*) tl
          else
            loop (Text (String.make (n/2) '\\') :: r) [t](*???*) (Backslash 1 :: tl)

      | _ ->
            assert false
          

  and read_title n lexemes =
    assert false
      
  in
    List.rev (loop [] [] lexemes)

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
open Md_lexer

let parse lexemes =
  let rec loop (result: 'a list) (previous:Md_lexer.t list) (lexemes:Md_lexer.t list) =
    (* let result = reduce result in *)
    match previous, lexemes with
      | _, [] ->
          result

      (* hashes *)
      | ([Newline _]|[]), Hash n :: tl -> (* hash titles *)
          read_title n tl
      | _, (Hash _ as t) :: tl -> (* hash -- no title *)
          loop (`Text (string_of_t t) :: result) [t] tl

      (* spaces *)
      | ([Newline _]|[]), Space n :: Star x :: tl -> 
          assert false
      | _, Space n :: tl ->
          assert false

      (* stars *)
      | _, Star 1 :: (e::_ as tl) -> (* * *)
          assert (match e with Star _ -> false| _ -> true);
          assert false
          
            
      (* backslashes *)
      | _, Backslash 1 :: (Backquote 1 as t) :: tl -> (* \` *)
          loop (`Text ("`") :: result) [t] tl
      | _, Backslash 1 :: Backquote n :: tl -> assert (n > 1); (* \````... *)
          loop (`Text ("`") :: result) [Backslash 1; Backquote 1] (Backquote (n-1) :: tl)
      | _, Backslash 1 :: (Star 1 as t) :: tl -> (* \* *)
          loop (`Text ("*") :: result) [t] tl
      | _, Backslash 1 :: Star n :: tl -> assert (n > 1); (* \****... *)
          loop (`Text ("*") :: result) [Backslash 1; Star 1] (Star (n-1) :: tl)

      | _, Backslash 1 :: (Underscore 1 as t) :: tl -> (* \_ *)
          loop (`Text ("_") :: result) [t] tl
      | _, Backslash 1 :: Underscore n :: tl -> assert (n > 1); (* \___... *)
          loop (`Text ("_") :: result) [Backslash 1; Underscore 1] (Underscore (n-1) :: tl)

      | _, Backslash 1 :: (Obrace 1 as t) :: tl -> (* \{ *)
          loop (`Text ("{") :: result) [t] tl
      | _, Backslash 1 :: Obrace n :: tl -> assert (n > 1); (* \{{{... *)
          loop (`Text ("{") :: result) [Backslash 1; Obrace 1] (Obrace (n-1) :: tl)
      | _, Backslash 1 :: (Cbrace 1 as t) :: tl -> (* \} *)
          loop (`Text ("}") :: result) [t] tl
      | _, Backslash 1 :: Cbrace n :: tl -> assert (n > 1); (* \}}}... *)
          loop (`Text ("}") :: result) [Backslash 1; Cbrace 1] (Cbrace (n-1) :: tl)
      | _, Backslash 1 :: (Obracket 1 as t) :: tl -> (* \[ *)
          loop (`Text ("[") :: result) [t] tl
      | _, Backslash 1 :: Obracket n :: tl -> assert (n > 1); (* \[[[... *)
          loop (`Text ("[") :: result) [Backslash 1; Obracket 1] (Obracket (n-1) :: tl)
      | _, Backslash 1 :: (Cbracket 1 as t) :: tl -> (* \} *)
          loop (`Text ("]") :: result) [t] tl
      | _, Backslash 1 :: Cbracket n :: tl -> assert (n > 1); (* \}}}... *)
          loop (`Text ("]") :: result) [Backslash 1; Cbracket 1] (Cbracket (n-1) :: tl)
      | _, Backslash 1 :: (Oparenthesis 1 as t) :: tl -> (* \( *)
          loop (`Text ("(") :: result) [t] tl
      | _, Backslash 1 :: Oparenthesis n :: tl -> assert (n > 1); (* \(((... *)
          loop (`Text ("(") :: result) [Backslash 1; Oparenthesis 1] (Oparenthesis (n-1) :: tl)
      | _, Backslash 1 :: (Cparenthesis 1 as t) :: tl -> (* \) *)
          loop (`Text (")") :: result) [t] tl
      | _, Backslash 1 :: Cparenthesis n :: tl -> assert (n > 1); (* \)))... *)
          loop (`Text (")") :: result) [Backslash 1; Cparenthesis 1] (Cparenthesis (n-1) :: tl)
      | _, Backslash 1 :: (Plus 1 as t) :: tl -> (* \+ *)
          loop (`Text ("+") :: result) [t] tl
      | _, Backslash 1 :: Plus n :: tl -> assert (n > 1); (* \+++... *)
          loop (`Text ("+") :: result) [Backslash 1; Plus 1] (Plus (n-1) :: tl)
      | _, Backslash 1 :: (Minus 1 as t) :: tl -> (* \- *)
          loop (`Text ("-") :: result) [t] tl
      | _, Backslash 1 :: Minus n :: tl -> assert (n > 1); (* \---... *)
          loop (`Text ("-") :: result) [Backslash 1; Minus 1] (Minus (n-1) :: tl)
      | _, Backslash 1 :: (Dot 1 as t) :: tl -> (* \. *)
          loop (`Text (".") :: result) [t] tl
      | _, Backslash 1 :: Dot n :: tl -> assert (n > 1); (* \....... *)
          loop (`Text (".") :: result) [Backslash 1; Dot 1] (Dot (n-1) :: tl)
      | _, Backslash 1 :: (Exclamation 1 as t) :: tl -> (* \! *)
          loop (`Text ("!") :: result) [t] tl
      | _, Backslash 1 :: Exclamation n :: tl -> assert (n > 1); (* \!!!... *)
          loop (`Text ("!") :: result) [Backslash 1; Exclamation 1] (Exclamation (n-1) :: tl)

            
      | _, (Backslash n as t) :: tl -> (* \\\\... *)
          if n mod 2 = 0 then
            loop (`Text (String.make (n/2) '\\') :: result) [t](*???*) tl
          else
            loop (`Text (String.make (n/2) '\\') :: result) [t](*???*) (Backslash 1 :: tl)

      | _ ->
            assert false
          

  and read_title n lexemes =
    assert false
      
  in 
    assert false

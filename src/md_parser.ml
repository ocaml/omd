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
  let rec loop (result: 'a list) previous (lexemes:Md_lexer.t list) =
    match previous, lexemes with
      | _, [] ->
          result

      (* hashes *)
      | Newline _, Hash n :: tl -> (* hash titles *)
          read_title n tl
      | _, (Hash _ as t) :: tl -> (* hash -- no title *)
          loop (`Text (string_of_t t) :: result) t tl
            
      (* backslashes *)
      | _, Backslash 1 :: (Backquote 1 as t) :: tl -> (* \` *)
          loop (`Text ("`") :: result) t tl
      | _, Backslash 1 :: Backquote n :: tl -> assert (n > 1); (* \````... *)
          loop (`Text ("`") :: result) (Backquote 1) (Backquote (n-1) :: tl)
            
      | _, (Backslash n as t) :: tl -> (* \\\\... *)
          if n mod 2 = 0 then
            loop (`Text (String.make (n/2) '\\') :: result) t(*???*) tl
          else
            loop (`Text (String.make (n/2) '\\') :: result) t(*???*) (Backslash 1 :: tl)

      | _ ->
            assert false
          

  and read_title n lexemes =
    assert false
      
  in 
    assert false

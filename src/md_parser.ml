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
    | Paragraph of md
    | Text of string
    | Emph of md
    | Bold of md
    | Ul of li list
    | Ol of li list
    | Sp of int (* spaces *)
    | Code of string (* html entities are to be converted *later* *)
    | Br
    | Hr
    | Html of string
and li = Li of md
and md = md_element list
    

open Md_lexer


(** [emph_or_bold (n:int) (r:md list) (l:Md_lexer.t list)] 
    returns [] if not (emph and/or bold),
    else returns the contents intended to be formatted,
    along with the rest of the stream that hasn't been processed. *)
let rec emph_or_bold (n:int) (l:Md_lexer.t list) : (Md_lexer.t list * Md_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:Md_lexer.t list) = function
    | [] ->
        [], l
    | (Newline|Newlines _) :: tl ->
        [], l
    | (Star as t) :: tl ->
        if n = 1 then
          (List.rev result), tl
        else
          loop (t :: result) tl
    | ((Stars x) as t) :: tl ->
        if n = x+2 then
          (List.rev result), tl
        else
          loop (t :: result) tl
    | t::tl ->
        loop (t :: result) tl
  in loop [] l


(* let emph_or_bold n l = failwith "" *)

type tmp_list = element list
and element   = Item of Md_lexer.t list

  

  
(* let icode r p l = failwith "" *)


(* let spaces n r previous l = failwith "" *)


let      xspaces = ref None
let      xnew_list = ref None
let      xicode = ref None
let      xmain_loop = ref None


let parse lexemes =
  let rec main_loop (r:md) (previous:Md_lexer.t list) (lexemes:Md_lexer.t list) =
    match previous, lexemes with

      (* no more to process *)
      | _, [] -> (* return the result (/!\ it has to be reversed as some point) *)
          r

      (* hashes *)
      | ([]|[(Newline|Newlines _)]), Hashs n :: tl -> (* hash titles *)
          read_title (n+2) tl
      | ([]|[(Newline|Newlines _)]), Hash :: tl -> (* hash titles *)
          read_title 1 tl
      | _, ((Hash|Hashs _) as t) :: tl -> (* hash -- no title *)
          main_loop (Text(string_of_t t) :: r) [t] tl

      (* spaces *)
      | _, ((Space|Spaces _) as t) :: tl -> (* too many cases to be handled here *)
          let r, p, l = spaces (fst (length t)) r previous tl in
            main_loop r p l

      (* stars *)
      | _, (Star as t) :: tl -> (* one "orphan" star, or emph *)
          begin match emph_or_bold 1 tl with
            | [], _      -> main_loop (Text(string_of_t t) :: r) [t] tl
            | x , new_tl -> main_loop (Emph(rev_main_loop [] [t] x) :: r) [t] new_tl
          end
      | _, (Stars((0|1) as n) as t) :: tl -> (* 2 or 3 "orphan" stars, or emph/bold *)
          begin match emph_or_bold (n+2) tl with
            | [], _ ->
                if n = 0 then
                  main_loop (Text(string_of_t t) :: r) [t] tl
                else
                  main_loop (Text(string_of_t t) :: r) [t] tl
            | x, new_tl ->
                if n = 0 then
                  main_loop (Bold(rev_main_loop [] [t] x) :: r) [t] new_tl
                else
                  main_loop (Emph([Bold(rev_main_loop [] [t] x)]) :: r) [t] new_tl
          end

      (* backslashes *)
      | _, Backslash :: (Backquote as t) :: tl -> (* \` *)
          main_loop (Text ("`") :: r) [t] tl
      | _, Backslash :: Backquotes 0 :: tl -> (* \````... *)
          main_loop (Text ("`") :: r) [Backslash; Backquote] (Backquote :: tl)
      | _, Backslash :: Backquotes n :: tl -> assert (n >= 0); (* \````... *)
          main_loop (Text ("`") :: r) [Backslash; Backquote] (Backquotes (n-1) :: tl)
      | _, Backslash :: (Star as t) :: tl -> (* \* *)
          main_loop (Text ("*") :: r) [t] tl
      | _, Backslash :: Stars 0 :: tl -> (* \****... *)
          main_loop (Text ("*") :: r) [Backslash; Star] (Star :: tl)
      | _, Backslash :: Stars n :: tl -> assert (n >= 0); (* \****... *)
          main_loop (Text ("*") :: r) [Backslash; Star] (Stars (n-1) :: tl)
      | _, Backslash :: (Underscore as t) :: tl -> (* \_ *)
          main_loop (Text ("_") :: r) [t] tl
      | _, Backslash :: Underscores 0 :: tl -> (* \___... *)
          main_loop (Text ("_") :: r) [Backslash; Underscore] (Underscore :: tl)
      | _, Backslash :: Underscores n :: tl -> assert (n >= 0); (* \___... *)
          main_loop (Text ("_") :: r) [Backslash; Underscore] (Underscores (n-1) :: tl)
      | _, Backslash :: (Obrace as t) :: tl -> (* \{ *)
          main_loop (Text ("{") :: r) [t] tl
      | _, Backslash :: Obraces 0 :: tl -> (* \{{{... *)
          main_loop (Text ("{") :: r) [Backslash; Obrace] (Obrace :: tl)
      | _, Backslash :: Obraces n :: tl -> assert (n >= 0); (* \{{{... *)
          main_loop (Text ("{") :: r) [Backslash; Obrace] (Obraces (n-1) :: tl)
      | _, Backslash :: (Cbrace as t) :: tl -> (* \} *)
          main_loop (Text ("}") :: r) [t] tl
      | _, Backslash :: Cbraces 0 :: tl -> (* \}}}... *)
          main_loop (Text ("}") :: r) [Backslash; Cbrace] (Cbrace :: tl)
      | _, Backslash :: Cbraces n :: tl -> assert (n >= 0); (* \}}}... *)
          main_loop (Text ("}") :: r) [Backslash; Cbrace] (Cbraces (n-1) :: tl)
      | _, Backslash :: (Obracket as t) :: tl -> (* \[ *)
          main_loop (Text ("[") :: r) [t] tl
      | _, Backslash :: Obrackets 0 :: tl -> (* \[[[... *)
          main_loop (Text ("[") :: r) [Backslash; Obracket] (Obracket :: tl)
      | _, Backslash :: Obrackets n :: tl -> assert (n >= 0); (* \[[[... *)
          main_loop (Text ("[") :: r) [Backslash; Obracket] (Obrackets (n-1) :: tl)
      | _, Backslash :: (Cbracket as t) :: tl -> (* \} *)
          main_loop (Text ("]") :: r) [t] tl
      | _, Backslash :: Cbrackets 0 :: tl -> (* \}}}... *)
          main_loop (Text ("]") :: r) [Backslash; Cbracket] (Cbracket :: tl)
      | _, Backslash :: Cbrackets n :: tl -> assert (n >= 0); (* \}}}... *)
          main_loop (Text ("]") :: r) [Backslash; Cbracket] (Cbrackets (n-1) :: tl)
      | _, Backslash :: (Oparenthesis as t) :: tl -> (* \( *)
          main_loop (Text ("(") :: r) [t] tl
      | _, Backslash :: Oparenthesiss 0 :: tl -> (* \(((... *)
          main_loop (Text ("(") :: r) [Backslash; Oparenthesis] (Oparenthesis :: tl)
      | _, Backslash :: Oparenthesiss n :: tl -> assert (n >= 0); (* \(((... *)
          main_loop (Text ("(") :: r) [Backslash; Oparenthesis] (Oparenthesiss (n-1) :: tl)
      | _, Backslash :: (Cparenthesis as t) :: tl -> (* \) *)
          main_loop (Text (")") :: r) [t] tl
      | _, Backslash :: Cparenthesiss 0 :: tl -> (* \)))... *)
          main_loop (Text (")") :: r) [Backslash; Cparenthesis] (Cparenthesis :: tl)
      | _, Backslash :: Cparenthesiss n :: tl -> assert (n >= 0); (* \)))... *)
          main_loop (Text (")") :: r) [Backslash; Cparenthesis] (Cparenthesiss (n-1) :: tl)
      | _, Backslash :: (Plus as t) :: tl -> (* \+ *)
          main_loop (Text ("+") :: r) [t] tl
      | _, Backslash :: Pluss 0 :: tl -> (* \+++... *)
          main_loop (Text ("+") :: r) [Backslash; Plus] (Plus :: tl)
      | _, Backslash :: Pluss n :: tl -> assert (n >= 0); (* \+++... *)
          main_loop (Text ("+") :: r) [Backslash; Plus] (Pluss (n-1) :: tl)
      | _, Backslash :: (Minus as t) :: tl -> (* \- *)
          main_loop (Text ("-") :: r) [t] tl
      | _, Backslash :: Minuss 0 :: tl -> (* \---... *)
          main_loop (Text ("-") :: r) [Backslash; Minus] (Minus :: tl)
      | _, Backslash :: Minuss n :: tl -> assert (n >= 0); (* \---... *)
          main_loop (Text ("-") :: r) [Backslash; Minus] (Minuss (n-1) :: tl)
      | _, Backslash :: (Dot as t) :: tl -> (* \. *)
          main_loop (Text (".") :: r) [t] tl
      | _, Backslash :: Dots 0 :: tl -> (* \....... *)
          main_loop (Text (".") :: r) [Backslash; Dot] (Dot :: tl)
      | _, Backslash :: Dots n :: tl -> assert (n >= 0); (* \....... *)
          main_loop (Text (".") :: r) [Backslash; Dot] (Dots (n-1) :: tl)
      | _, Backslash :: (Exclamation as t) :: tl -> (* \! *)
          main_loop (Text ("!") :: r) [t] tl
      | _, Backslash :: Exclamations 0 :: tl -> (* \!!!... *)
          main_loop (Text ("!") :: r) [Backslash; Exclamation] (Exclamation :: tl)
      | _, Backslash :: Exclamations n :: tl -> assert (n >= 0); (* \!!!... *)
          main_loop (Text ("!") :: r) [Backslash; Exclamation] (Exclamations (n-1) :: tl)
            
      | _, (Backslashs n as t) :: tl -> (* \\\\... *)
          if n mod 2 = 0 then
            main_loop (Text (String.make ((n-2)/2) '\\') :: r) [t](*???*) tl
          else
            main_loop (Text (String.make ((n-2)/2) '\\') :: r) [t](*???*) (Backslash :: tl)
              (* | _, Backslash ::  *)
      | (_, Backslash::[]) ->
          main_loop (Text "\\" :: r) [] []
      | (_,
        (Ampersand|Ampersands _|At|Ats _|Backquote|Backquotes _|Bar|Bars _|Caret|
            Carets _|Cbrace|Cbraces _|Colon|Colons _|Cparenthesis|Cparenthesiss _|
                Cbracket|Cbrackets _|Dollar|Dollars _|Dot|Dots _|Doublequote|Doublequotes _|
                    Exclamation|Exclamations _|Equal|Equals _|Greaterthan|Greaterthans _|
                        Lessthan|Lessthans _|Minus|Minuss _|Newline|Newlines _|Number _|Obrace|
                            Obraces _|Oparenthesis|Oparenthesiss _|Obracket|Obrackets _|Percent|
                                Percents _|Plus|Pluss _|Question|Questions _|Quote|Quotes _|Return|Returns _|
                                    Semicolon|Semicolons _|Slash|Slashs _|Tab|Tabs _|Tilde|Tildes _|Underscore|
                                        Underscores _|Word _)::_)
      | (_, Stars _::_)
      | (_, Backslash::(Ampersand|Ampersands _|At|Ats _|Backslash|Backslashs _|Bar|Bars _|Caret|
            Carets _|Colon|Colons _|Dollar|Dollars _|Doublequote|Doublequotes _|Equal|
                Equals _|Greaterthan|Greaterthans _|Hash|Hashs _|Lessthan|Lessthans _|
                    Newline|Newlines _|Number _|Percent|Percents _|Question|Questions _|Quote|
                        Quotes _|Return|Returns _|Semicolon|Semicolons _|Slash|Slashs _|Space|
                            Spaces _|Tab|Tabs _|Tilde|Tildes _|Word _)::_)
        -> assert false (* not yet implemented *)            

  and read_title n lexemes =
    assert false

  and rev_main_loop (r: md) (previous:Md_lexer.t list) (lexemes:Md_lexer.t list) =
    List.rev (main_loop r previous lexemes)

  and icode (r:md) (p:Md_lexer.t list) (l:Md_lexer.t list) : md * Md_lexer.t list * Md_lexer.t list =
    (** indented code:
        returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
    let icode (r:md) previous l = (* icode's internal implementation is not recursive. *)
      let accu = Buffer.create 42 in
      let rec loop = function
        | (([]|[Newline|Newlines _]) as p), (((Space|Spaces(0|1))::_) as tl) ->  (* 1, 2 or 3 spaces. *)
            Code (Buffer.contents accu)::r, p, tl (* -> Return what's been found as code because it's no more code. *)
        | ([]|[Newline|Newlines _]), (Spaces(n) as t)::tl -> (* At least 4 spaces, it's still code. *)
            Buffer.add_string accu (String.make (n-2) ' ');
            loop ([t], tl)
        | ([(Newline|Newlines _)] as p), not_spaces::tl -> (* stop *)
            Code (Buffer.contents accu)::r, p, tl (* -> Return what's been found as code because it's no more code. *)
        | _, e::tl ->
            Buffer.add_string accu (string_of_t e); (* html entities are to be converted later! *)
            loop ([e], tl)
        | p, [] ->
            Code (Buffer.contents accu)::r, p, []
      in loop ([Newlines 0], l)
    in icode (r:md) p l

  (** new_list: returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
  and new_list (r:md) (p:Md_lexer.t list) (l:Md_lexer.t list) : (md * Md_lexer.t list * Md_lexer.t list) =
    begin
      let list_hd e = match e with hd::_ -> hd | _ -> assert false in
      let rec loop (result:((int list)*(Md_lexer.t list))list) (curr_item:Md_lexer.t list) (indents:int list) = function
          (* first loop: return the list of (indentation level * item) *)
        | Newline :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl
        | Newline :: (Number _) :: Dot :: (Space|Spaces _) :: tl ->
            loop ((indents,(curr_item:Md_lexer.t list))::result) [] (0::indents) tl
        | Newline :: Space :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl 
        | Newline :: Space :: Number _ :: Dot :: (Space|Spaces _) :: tl ->
            loop ((indents,curr_item)::result) [] (1::indents) tl
        | Newline :: ((Spaces(x) :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl) as p) 
        | Newline :: ((Spaces(x) :: Number _ :: Dot :: (Space|Spaces _) :: tl) as p) ->
            if x+2 > list_hd indents + 4 then
              begin (* a single new line & too many spaces -> *not* a new list item. *)
                loop result curr_item indents p (* p is what follows the new line *)
              end
            else
              begin (* a new list item, set previous current item as a complete item *)
                loop ((indents,curr_item)::result) [] ((x+2)::indents) tl
              end
        | ([] | (Newlines(_) :: _)) as l -> 
            (* if an empty line appears, then it's the end of the list(s). *)
            ((result:((int list)*(Md_lexer.t list)) list), (l: Md_lexer.t list))
        | (Newline :: e :: tl)  (* adding e to the current item *)
        | e :: tl -> 
            loop result (e::curr_item) indents tl
      in
      let rec loop2 (tmp:(int list * Md_lexer.t list) list) (curr_indent:int) (accu:li list) =
        match tmp with
          | ((i::indents), item) :: tl ->
              if i = curr_indent then
                loop2 tl curr_indent (Li(main_loop [] [Space;Star] item)::accu)
              else if i > curr_indent then (* new sub list *)
                loop2 [] i (Li(main_loop [] [Space;Star] item)::[])
              else (* i < curr_indent *)
                if List.mem i indents then (* i < curr_indent && List.mem i indents *)
                  (Ul accu)::(loop2 tmp (-1) [])
              else (* i < curr_indent && not(List.mem i indents) *)
                loop2 [] i (Li(main_loop [] [Space;Star] item)::[])
          | [] ->
              Ul accu::loop2 tmp (-1) []
          | ([], _) :: _ -> assert false
      in
      let tmp_r, new_l = loop [] [] [] l in
      let e:md = 
        loop2 tmp_r (-1) []
      in
        (e@(r:md)), [], new_l
    end


  (** spaces: returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
  and spaces n r p l =
    let spaces n r previous l = match n, previous, l with
      | (1|2|3), ([]|[(Newline|Newlines _)]), (Star|Minus|Plus)::(Space|Spaces _)::tl  (* unordered list *)
      | (1|2|3), ([]|[(Newline|Newlines _)]), (Number _)::Dot::(Space|Spaces _)::tl -> (* ordered list *)
          begin
            Obj.magic (new_list r [] (Newline::make_space n::l))
          end
      | _, ([]|[(Newlines _)]), _ -> (* n>=4, indented code *)
          Obj.magic (icode r previous (make_space n :: l))
      | 1, _, _ ->
          (Sp 1::r), [Space], l
      | n, _, _ -> assert (n>1);
          (Sp n::r), [Spaces (n-2)], l
    in spaces n r p l


  in
    ( (* dummy *)
      xspaces := Some spaces;
      xnew_list := Some new_list;
      xicode := Some icode;
      xmain_loop := Some main_loop;
    );
    rev_main_loop [] [] lexemes

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

let debug = try ignore(Sys.getenv "DEBUG"); true with _ -> false

let raise = 
  if debug then
    (fun e -> Printf.eprintf "Exception raised: %s\n%!" (Printexc.to_string e) ; raise e)
  else
    raise

type tag = Maybe_h1 | Maybe_h2

type tmp_list = element list
and element   = Item of tag Md_lexer.t list


open Md_backend
open Md_lexer

module StringSet : sig
  type elt = string
  type t
  val mem : elt -> t -> bool
  val of_list : elt list -> t
end = struct
  include Set.Make(struct type t = string let compare = String.compare end)
  let of_list l = List.fold_left (fun r e -> add e r) empty l
end

let htmlentities_set = StringSet.of_list (* This list should be checked... *)
  (* list extracted from: http://www.ascii.cl/htmlcodes.htm *)
  ["eth";  "ntilde";  "ograve";  "oacute";  "ocirc";  "otilde"; "ouml";
   "divide"; "oslash";  "ugrave"; "uacute"; "ucirc";  "uuml"; "yacute";
   "thorn";  "yuml";  "agrave";  "aacute"; "acirc";  "atilde";  "auml";
   "aring";  "aelig"; "ccedil";  "egrave";  "eacute"; "ecirc";  "euml";
   "igrave";  "iacute";  "icirc";  "iuml"; "ETH";  "Ntilde";  "Ograve";
   "Oacute";  "Ocirc"; "Otilde";  "Ouml"; "times";  "Oslash"; "Ugrave";
   "Uacute";  "Ucirc"; "Uuml";  "Yacute";  "THORN"; "szlig";  "Agrave";
   "Aacute";  "Acirc"; "Atilde";  "Auml";  "Aring"; "AElig";  "Ccedil";
   "Egrave";  "Eacute"; "Ecirc";  "Euml"; "Igrave";  "Iacute"; "Icirc";
   "Iuml"; "deg";  "plusmn"; "sup2"; "sup3";  "acute"; "micro"; "para";
   "middot";  "cedil";  "sup1";  "ordm"; "raquo";  "frac14";  "frac12";
   "frac34";  "iquest";  "nbsp";  "iexcl"; "cent";  "pound";  "curren";
   "yen";  "brvbar";  "sect"; "uml";  "copy";  "ordf"; "laquo";  "not";
   "shy"; "reg"; "macr"; "quot"; "amp"; "euro"; ]


let rec is_blank = function
  | (Space | Spaces _ | Newline | Newlines _) :: tl ->
      is_blank tl
  | [] -> true
  | _ -> false

(** [emph_or_bold (n:int) (r:md list) (l:Md_lexer.t list)]
    returns [] if not (emph and/or bold),
    else returns the contents intended to be formatted,
    along with the rest of the stream that hasn't been processed. *)
let emph_or_bold (n:int) (l:tag Md_lexer.t list) : (tag Md_lexer.t list * tag Md_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:tag Md_lexer.t list) = function
    | [] ->
        [], l
    | (Newline|Newlines _) :: tl ->
        [], l
    | Backslash::Star::tl ->
        loop (Star::result) tl
    | Backslash::Stars 0::tl ->
        loop (Star::result) tl
    | Backslash::Stars n::tl ->
        loop (Star::result) (Stars(n-1)::tl)
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
  in
  let r, tl = loop [] l in
    if is_blank r then
      [], l
    else
      r, tl

(** [uemph_or_bold (n:int) (r:md list) (l:tag Md_lexer.t list)]
    returns [] if not (emph and/or bold),
    else returns the contents intended to be formatted,
    along with the rest of the stream that hasn't been processed. *)
let uemph_or_bold (n:int) (l:tag Md_lexer.t list) : (tag Md_lexer.t list * tag Md_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:tag Md_lexer.t list) = function
    | [] ->
        [], l
    | (Newline|Newlines _) :: tl ->
        [], l
    | Backslash::Underscore::tl ->
        loop (Underscore::result) tl
    | Backslash::Underscores 0::tl ->
        loop (Underscore::result) tl
    | Backslash::Underscores n::tl ->
        loop (Underscore::result) (Underscores(n-1)::tl)
    | (Underscore as t) :: tl ->
        if n = 1 then
          (List.rev result), tl
        else
          loop (t :: result) tl
    | ((Underscores x) as t) :: tl ->
        if n = x+2 then
          (List.rev result), tl
        else
          loop (t :: result) tl
    | t::tl ->
        loop (t :: result) tl
  in
  let r, tl = loop [] l in
    if is_blank r then
      [], tl
    else
      r, l


let gh_uemph_or_bold (n:int) (l:tag Md_lexer.t list) : (tag Md_lexer.t list * tag Md_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:tag Md_lexer.t list) = function
    | [] ->
        [], l
    | (Newline|Newlines _) :: tl ->
        [], l
    | Backslash::Underscore::tl ->
        loop (Underscore::result) tl
    | Backslash::Underscores 0::tl ->
        loop (Underscore::result) tl
    | Backslash::Underscores n::tl ->
        loop (Underscore::result) (Underscores(n-1)::tl)
    | (Underscore | Underscores _ as t) :: (Word _ as w) :: tl ->
          loop (w :: t :: result) tl
    | (Underscore as t) :: tl ->
        if n = 1 then
          (List.rev result), tl
        else
          loop (t :: result) tl
    | ((Underscores x) as t) :: tl ->
        if n = x+2 then
          (List.rev result), tl
        else
          loop (t :: result) tl
    | t::tl ->
        loop (t :: result) tl
  in
  let r, tl = loop [] l in
    if is_blank r then
      [], l
    else
      r, tl


let uemph_or_bold =
  if true then gh_uemph_or_bold else uemph_or_bold


(* let icode r p l = failwith "" *)


(* let spaces n r previous l = failwith "" *)

(* The few following lines are there for development purpose only. *)
let xspaces = ref None
let xnew_list = ref None
let xicode = ref None
let xmain_loop = ref None

let tag_setext l =
  let rec loop pl res = function (* pl = previous line *)
  | (Newline as e1)::(Equal|Equals _ as e2)::tl ->
      loop [] (e2::e1::pl@[Tag(Maybe_h1)]@res) tl
  | (Newline as e1)::(Minus|Minuss _ as e2)::tl ->
      loop [] (e2::e1::pl@[Tag(Maybe_h2)]@res) tl
  | Newline as e::tl ->
      loop [] (e::pl@res) tl
  | e::tl ->
      loop (e::pl) res tl
  | [] ->
      List.rev res
    in loop [] [] l

let setext_title l =
  let rec loop r = function
    | [] -> List.rev r, []
    | Newline::(Equal|Equals _|Minus|Minuss _)::tl -> List.rev r, tl
    | e::tl -> loop (e::r) tl
  in
    loop [] l

(** [hr_m l] returns [Some nl] where [nl] is the remaining of [l] if [l]
    contains a horizontal rule drawn with dashes. If it doesn't, then
    returns [None].*)
let hr_m l =
  let rec loop n = function
    | (Newline::tl) | ([] as tl) ->
        if n >= 3 then Some tl else None
    | (Space|Spaces _)::tl ->
        loop n tl
    | Minus::tl ->
        loop (n+1) tl
    | Minuss x::tl ->
        loop (x+2+n) tl
    | _::_ ->
        None
  in loop 0 l

(** [hr_s l] returns [Some nl] where [nl] is the remaining of [l] if [l]
    contains a horizontal rule drawn with dashes. If it doesn't, then
    returns [None].*)
let hr_s l =
  let rec loop n = function
    | ((Newline|Newlines _)::tl) | ([] as tl) ->
        if n >= 3 then Some tl else None
    | (Space|Spaces _)::tl ->
        loop n tl
    | Star::tl ->
        loop (n+1) tl
    | Stars x::tl ->
        loop (x+2+n) tl
    | _::_ ->
        None
  in loop 0 l


(* This function is intended to fix lists that "look wrong" in
   Markdown. First, nothing being "wrong" in Markdown means that what
   doesn't have a clear semantics has to be attributed one. Lists are
   based on indentation and sometimes it appears that we can't count
   on human beings to write only good looking Markdown. Second, it
   appeared hard to write a simple lists parser that would generate
   the tree that I expected to see. For those 2 reasons, I wrote this
   function that fixes what's generated at some point by this
   parser. It seems to work fine. 
   
   However, if this parser benefits from some optimizations at some
   point, one of them should probably be about getting rid of this
   function.
*)
let rec fix_lists = function
  | Ul []::tl ->
      fix_lists tl
  | Ol []::tl ->
      fix_lists tl
  | Ul (Li((Ul(_)::_) as l)::l2) :: tl
  | Ul (Li((Ol(_)::_) as l)::l2) :: tl ->
      fix_lists [Ul(l2)] @ fix_lists l @ fix_lists tl
  | Ol (Li((Ul(_)::_) as l)::l2) :: tl
  | Ol (Li((Ol(_)::_) as l)::l2) :: tl ->
      fix_lists [Ol(l2)] @ fix_lists l @ fix_lists tl
  | Ul l :: tl ->
      Ul (List.map (fun (Li e) -> Li(fix_lists e)) l) :: fix_lists tl
  | Ol l :: tl ->
      Ol (List.map (fun (Li e) -> Li(fix_lists e)) l) :: fix_lists tl
  | Img _ as i :: tl ->
      i :: fix_lists tl
  | Paragraph p :: tl ->
      Paragraph (fix_lists p) :: fix_lists tl
  | Text _ as e :: tl ->
      e::fix_lists tl
  | Emph e :: tl ->
      Emph(fix_lists e)::fix_lists tl
  | Bold e :: tl ->
      Bold(fix_lists e)::fix_lists tl
  | (Code _ | Br | Hr | Url (_, _, _) | Html _ as e) :: tl ->
      e::fix_lists tl
  | H1 e :: tl ->
      H1(fix_lists e)::fix_lists tl
  | H2 e :: tl ->
      H2(fix_lists e)::fix_lists tl
  | H3 e :: tl ->
      H3(fix_lists e)::fix_lists tl
  | H4 e :: tl ->
      H4(fix_lists e)::fix_lists tl
  | H5 e :: tl ->
      H5(fix_lists e)::fix_lists tl
  | H6 e :: tl ->
      H6(fix_lists e)::fix_lists tl
  | NL :: tl ->
      NL :: fix_lists tl
  | [] ->
      []

(* let fix_lists x = x *)
exception NL_exception
exception Premature_ending

let read_until_gt ?(no_nl=false) l =
  let rec loop accu = function
    | Greaterthan :: tl -> (List.rev (accu)), tl
    | Greaterthans 0 :: tl -> (List.rev (accu)), Greaterthan::tl
    | Greaterthans n :: tl -> (List.rev (accu)), Greaterthans(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
        if no_nl then raise NL_exception;
        loop (e::accu) tl
    | e::tl -> loop (e::accu) tl
    | [] -> raise Premature_ending
  in loop [] l

let read_until_lt ?(no_nl=false) l =
  let rec loop accu = function
    | Lessthan :: tl -> (List.rev (accu)), tl
    | Lessthans 0 :: tl -> (List.rev (accu)), Lessthan::tl
    | Lessthans n :: tl -> (List.rev (accu)), Lessthans(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
        if no_nl then raise NL_exception;
        loop (e::accu) tl
    | e::tl -> loop (e::accu) tl
    | [] -> raise Premature_ending
  in loop [] l


let read_until_cparenth ?(no_nl=false) l =
  let rec loop accu = function
    | Cparenthesis :: tl -> (List.rev (accu)), tl
    | Cparenthesiss 0 :: tl -> (List.rev (accu)), Cparenthesis::tl
    | Cparenthesiss n :: tl -> (List.rev (accu)), Cparenthesiss(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
        if no_nl then raise NL_exception;
        loop (e::accu) tl
    | e::tl -> loop (e::accu) tl
    | [] -> raise Premature_ending
  in loop [] l

let read_until_dq ?(no_nl=false) l =
  let rec loop accu = function
    | Doublequote :: tl -> (List.rev (accu)), tl
    | Doublequotes 0 :: tl -> (List.rev (accu)), Doublequote::tl
    | Doublequotes n :: tl -> (List.rev (accu)), Doublequotes(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
        if no_nl then raise NL_exception;
        loop (e::accu) tl
    | e::tl -> loop (e::accu) tl
    | [] -> raise Premature_ending
  in loop [] l

let read_until_space ?(no_nl=false) l =
  let rec loop accu = function
    | Space :: tl -> (List.rev (accu)), tl
    | Spaces 0 :: tl -> (List.rev (accu)), Space::tl
    | Spaces n :: tl -> (List.rev (accu)), Spaces(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
        if no_nl then raise NL_exception;
        loop (e::accu) tl
    | e::tl -> loop (e::accu) tl
    | [] -> raise Premature_ending
  in loop [] l

let read_until_cbracket ?(no_nl=false) l =
  let rec loop accu = function
    | Cbracket :: tl -> (List.rev (accu)), tl
    | Cbrackets 0 :: tl -> (List.rev (accu)), Cbracket::tl
    | Cbrackets n :: tl -> (List.rev (accu)), Cbrackets(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
        if no_nl then raise NL_exception;
        loop (e::accu) tl
    | e::tl -> loop (e::accu) tl
    | [] -> raise Premature_ending
  in loop [] l


let main_parse lexemes =
  let rec main_loop (r:md) (previous:tag Md_lexer.t list) (lexemes:tag Md_lexer.t list) =
    if debug then Printf.eprintf "main_loop p=(%s) l=(%s)\n%!" (destring_of_tl previous) (destring_of_tl lexemes);
    match previous, lexemes with

      (* no more to process *)
      | _, [] -> (* return the result (/!\ it has to be reversed as some point) *)
          r

      (* maybe tags*)
      | ([]|[Newline|Newlines _]), Tag(Maybe_h1)::tl ->
          begin match setext_title tl with
            | [], _ ->
                main_loop [] [] tl
            | title, tl ->
                let title = H1(rev_main_loop [] [] title) in
                  main_loop (title::r) [] tl
          end
      | ([]|[Newline|Newlines _]), Tag(Maybe_h2)::tl ->
          begin match setext_title tl with
            | [], _ ->
                main_loop [] [] tl
            | title, tl ->
                let title = H2(rev_main_loop [] [] title) in
                  main_loop (title::r) [] tl
          end
      | _, Tag(Maybe_h1|Maybe_h2)::tl ->
          assert false

      (* minus *)
      | ([]|[Newline|Newlines _]), (Minus|Minuss _)::(Space|Spaces _)::_ -> (* maybe hr *)
          begin match hr_m lexemes with
            | None -> (* no hr, so it's a list *)
                begin match new_list r [] (Newline::lexemes) with
                  | md, new_p, new_l -> main_loop (md@r) new_p new_l
                end
            | Some l -> (* hr *)
                main_loop (Hr::r) [Newline] l
          end
      | ([]|[Newline|Newlines _]), (Minus|Minuss _ as m)::tl ->
          begin match hr_m lexemes with
            | None -> (* no hr, but it's not a list *)
                main_loop (Text(string_of_t m)::r) [Minus] tl
            | Some l -> (* hr *)
                main_loop (Hr::r) [Newline] l
          end

      (* hashes *)
      | ([]|[(Newline|Newlines _)]), Hashs n :: tl -> (* hash titles *)
          begin match read_title (n+2) r previous tl with
            | r, p, l -> main_loop r p l
          end
      | ([]|[(Newline|Newlines _)]), Hash :: tl -> (* hash titles *)
          begin match read_title 1 r previous tl with
            | r, p, l -> main_loop r p l
          end
      | _, ((Hash|Hashs _) as t) :: tl -> (* hash -- no title *)
          main_loop (Text(string_of_t t) :: r) [t] tl

      (* spaces after a newline: could lead to hr *)
      | ([]|[Newline|Newlines _]), ((Space|Spaces _) as t) :: tl ->
          begin match hr_s tl with
            | None ->
                begin match hr_m tl with
                  | None ->
                      let r, p, l = spaces (fst (length t)) r previous tl in
                        main_loop r p l
                  | Some l ->
                      main_loop (Hr::r) [Newline] l
                end
            | Some l ->
                main_loop (Hr::r) [Newline] l
          end

      (* spaces anywhere *)
      | _, ((Space|Spaces _) as t) :: tl -> (* too many cases to be handled here *)
          let r, p, l = spaces (fst (length t)) r previous tl in
            main_loop r p l

      (* underscores *)
      | _, (Underscore as t) :: tl -> (* one "orphan" underscore, or emph *)
          begin match uemph_or_bold 1 tl with
            | [], _      -> main_loop (Text(string_of_t t) :: r) [t] tl
            | x , new_tl -> main_loop (Emph(rev_main_loop [] [t] x) :: r) [t] new_tl
          end
      | _, (Underscores((0|1) as n) as t) :: tl -> (* 2 or 3 "orphan" stars, or emph/bold *)
          begin match uemph_or_bold (n+2) tl with
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

      (* stars *)
      | ([]|[(Newline|Newlines _)]), Star :: (Space|Spaces _) :: _ -> (* maybe hr or new list *)
          begin match hr_s lexemes with
            | Some l ->
                main_loop (Hr::r) [Newline] l
            | None ->
                begin match new_list r [] (Newline::lexemes) with
                  | md, new_p, new_l -> main_loop (md@r) new_p new_l
                end
          end
      | ([]|[(Newline|Newlines _)]), Stars _ :: _ when (not (hr_s lexemes = None)) -> (* hr *)
          begin match hr_s lexemes with
            | Some l -> main_loop (Hr::r) [Newline] l
            | None -> assert false
          end
      | ([]|[(Newline|Newlines _)]), Star :: tl -> (* maybe hr *)
          begin match hr_s lexemes with
            | Some l -> main_loop (Hr::r) [Newline] l
            | None ->
                main_loop (Text "*"::r) [Star] tl
          end
      | _, (Star as t) :: tl -> (* one "orphan" star, or emph // can't be hr *)
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
      | _, Backslash :: (Hash as t) :: tl -> (* \# *)
          main_loop (Text ("#") :: r) [t] tl
      | _, Backslash :: Hashs 0 :: tl -> (* \###... *)
          main_loop (Text ("#") :: r) [Backslash; Hash] (Hash :: tl)
      | _, Backslash :: Hashs n :: tl -> assert (n >= 0); (* \###... *)
          main_loop (Text ("#") :: r) [Backslash; Hash] (Hashs (n-1) :: tl)
      | _, (Backslashs n as t) :: tl -> (* \\\\... *)
          if n mod 2 = 0 then
            main_loop (Text (String.make ((n-2)/2) '\\') :: r) [t] tl
          else
            main_loop (Text (String.make ((n-2)/2) '\\') :: r) [t] (Backslash :: tl)
      | _, Backslash::[] ->
          main_loop (Text "\\" :: r) [] []
      | _, Backslash::tl ->
          main_loop (Text "\\" :: r) [Backslash] tl

      | _, (Lessthan|Lessthans _ as lt)::((Word("http"|"https"|"ftp"|"ftps"|"ssh"|"afp"|"imap") as w)::Colon::Slashs(n)::tl as fallback) -> (* "semi-automatic" URLs *)
          let rec read_url accu = function
            | (Newline|Newlines _|Return|Returns _)::tl ->
                None
            | Greaterthan::tl ->
                let url =
                  (match lt with Lessthans 0 -> "<" | Lessthans n -> String.make (n-2) '<' | _ -> "")
                  ^ (string_of_t w) ^ "://"
                  ^ (if n = 0 then "" else String.make (n-2) '/')
                  ^ string_of_tl (List.rev accu)
                in Some(url, tl)
            | x::tl ->
                read_url (x::accu) tl
            | [] ->
                None
          in
            begin match read_url [] tl with
              | Some(url, new_tl) ->
                  main_loop (Url(url,url,"")::r) [] new_tl
              | None ->
                  main_loop (Text(string_of_t lt)::r) [Lessthan] fallback
            end

      (* Email addresses are not so simple to handle because of the
         possible presence of characters such as '-', '_', '+' and '.'.
         Maybe they should be framed at lexing time. If at parsing time,
         it should probably be _here_. *)

      | _, Word w::tl ->
          main_loop (Text w :: r) [Word w] tl
      | _, [Newline] ->
          NL::r
      | _, Ampersand::((Word w::((Semicolon|Semicolons _) as s)::tl) as tl2) ->
          if StringSet.mem w htmlentities_set then
            begin match s with
              | Semicolon ->
                  main_loop (Html("&"^w^";")::r) [s] tl
              | Semicolons 0 ->
                  main_loop (Html("&"^w^";")::r) [s] (Semicolon::tl)
              | Semicolons n ->
                  main_loop (Html("&"^w^";")::r) [s] (Semicolons(n-1)::tl)
              | _ -> assert false
            end
          else
            main_loop (Html("&amp;")::r) [] tl2
      | _, Ampersand::((Hash::Number w::((Semicolon|Semicolons _) as s)::tl) as tl2) ->
          if String.length w <= 4 then
            begin match s with
              | Semicolon ->
                  main_loop (Html("&#"^w^";")::r) [s] tl
              | Semicolons 0 ->
                  main_loop (Html("&#"^w^";")::r) [s] (Semicolon::tl)
              | Semicolons n ->
                  main_loop (Html("&#"^w^";")::r) [s] (Semicolons(n-1)::tl)
              | _ -> assert false
            end
          else
            main_loop (Html("&amp;")::r) [] tl2
      | _, Ampersand::tl ->
          main_loop (Html("&amp;")::r) [Ampersand] tl
      | _, Ampersands(0)::tl ->
          main_loop (Html("&amp;")::r) [] (Ampersand::tl)
      | _, Ampersands(n)::tl ->
          main_loop (Html("&amp;")::r) [] (Ampersands(n-1)::tl)
      | _, (Backquote|Backquotes _)::_ ->
          begin match bcode r previous lexemes with
            | r, p, l -> main_loop r p l
          end
      | ([]|[Newline|Newlines _]), (Lessthan|Lessthans _ as opening)::
          (* Block HTML. *)
          Word("a"|"abbr"|"acronym"|"address"|"applet"|"area"|"article"|"aside"
          |"audio"|"b"|"base"|"basefont"|"bdi"|"bdo"|"big"|"blockquote" (* |"body" *)
          |"br"|"button"|"canvas"|"caption"|"center"|"cite"|"code"|"col"
          |"colgroup"|"command"|"datalist"|"dd"|"del"|"details"|"dfn"|"dialog"
          |"dir"|"div"|"dl"|"dt"|"em"|"embed"|"fieldset"|"figcaption"|"figure"
          |"font"|"footer"|"form"|"frame"|"frameset"|"h1" (* |"head" *) |"header"|"hr"
                (* |"html" *) |"i"|"iframe"|"img"|"input"|"ins"|"kbd"|"keygen"|"label"
          |"legend"|"li" (* |"link"| *) |"map"|"mark"|"menu" (* |"meta" *) |"meter"|"nav"
          |"noframes"|"noscript"|"object"|"ol"|"optgroup"|"option"|"output"|"p"
          |"param"|"pre"|"progress"|"q"|"rp"|"rt"|"ruby"|"s"|"samp"|"script"
          |"section"|"select"|"small"|"source"|"span"|"strike"|"strong"|"style"
          |"sub"|"summary"|"sup"|"table"|"tbody"|"td"|"textarea"|"tfoot"|"th"
          |"thead"|"time" (* |"title" *) |"tr"|"track"|"tt"|"u"|"ul"|"var"|"video"|"wbr" as tagname)
          ::((Space|Spaces _|Greaterthan|Greaterthans _) as x)
          ::tl ->
          let read_html() =
            let rec loop accu n = function
              | Lessthan::Slash::Word(tn)::Greaterthan::tl ->
                  if tn = tagname then
                    if n = 0 then
                      List.rev (Greaterthan::Word(tn)::Slash::Lessthan::accu), tl
                    else
                      loop (Greaterthan::Word(tn)::Slash::Lessthan::accu) (n-1) tl
                  else
                    loop (Greaterthan::Word(tn)::Slash::Lessthan::accu) n tl
              | Lessthan::Word(tn)::tl ->
                  if tn = tagname then
                    loop (Word(tn)::Lessthan::accu) (n+1) tl
                  else
                    loop (Word(tn)::Lessthan::accu) n tl
              | x::tl ->
                  loop (x::accu) n tl
              | [] ->
                  List.rev accu, []
            in
              loop [x;Word(tagname);Lessthan] 0 tl
          in
            begin
              let r = match opening with
                | Lessthan -> r
                | Lessthans 0 -> Text("<")::r
                | Lessthans n -> Text(String.make (n-3) '<')::r
                | _ -> assert false
              in
                match read_html() with
                  | html, tl -> main_loop (Html(string_of_tl html)::r) [Greaterthan] tl
            end
      | _, (Lessthan|Lessthans _ as opening)::
          (* inline HTML. *)
          (Word("a"|"abbr"|"acronym"|"address"|"applet"|"area"|"article"|"aside"
          |"audio"|"b"|"base"|"basefont"|"bdi"|"bdo"|"big"|"blockquote" (* |"body" *)
          |"br"|"button"|"canvas"|"caption"|"center"|"cite"|"code"|"col"
          |"colgroup"|"command"|"datalist"|"dd"|"del"|"details"|"dfn"|"dialog"
          |"dir"|"div"|"dl"|"dt"|"em"|"embed"|"fieldset"|"figcaption"|"figure"
          |"font"|"footer"|"form"|"frame"|"frameset"|"h1" (* |"head" *) |"header"|"hr"
                (* |"html" *) |"i"|"iframe"|"img"|"input"|"ins"|"kbd"|"keygen"|"label"
          |"legend"|"li" (* |"link" *) |"map"|"mark"|"menu" (* |"meta" *) |"meter"|"nav"
          |"noframes"|"noscript"|"object"|"ol"|"optgroup"|"option"|"output"|"p"
          |"param"|"pre"|"progress"|"q"|"rp"|"rt"|"ruby"|"s"|"samp"|"script"
          |"section"|"select"|"small"|"source"|"span"|"strike"|"strong"|"style"
          |"sub"|"summary"|"sup"|"table"|"tbody"|"td"|"textarea"|"tfoot"|"th"
          |"thead"|"time" (* |"title" *) |"tr"|"track"|"tt"|"u"|"ul"|"var"|"video"|"wbr" as tagname) as w)
          ::((Space|Spaces _|Greaterthan|Greaterthans _ as x)
             ::tl as l) ->
          let read_html() =
            let rec loop accu n = function
              | Lessthan::Word("img"|"br"|"hr" as tn)::tl -> (* self-closing tags *)
                  begin
                    if n = 0 then
                      match read_until_gt tl with
                        | b, tl -> (Word(Printf.sprintf "<%s%s>" tn (string_of_tl b))::accu), tl
                    else
                      match read_until_gt tl with
                        | b, tl -> loop (Word(Printf.sprintf "<%s%s>" tn (string_of_tl b))::accu) n tl
                  end               
              | Lessthan::Slash::Word(tn)::Greaterthan::tl -> (* </word> ... *)
                  if tn = tagname then
                    if n = 0 then
                      List.rev (Word(Printf.sprintf "</%s>" tn)::accu), tl
                    else
                      loop (Word(Printf.sprintf "</%s>" tn)::accu) (n-1) tl
                  else
                    loop (Word(Printf.sprintf "</%s>" tn)::accu) n tl
              | Lessthan::Word(tn)::tl -> (* <word... *)
                  begin
                    match read_until_gt tl with
                      | b, tl ->
                          if tn = tagname then
                            loop (Word(Printf.sprintf "<%s%s>" tn (string_of_tl b))::accu) (n+1) tl
                          else
                            loop (Word(Printf.sprintf "<%s%s>" tn (string_of_tl b))::accu) n tl
                  end
              | x::tl ->
                  loop (x::accu) n tl
              | [] ->
                  List.rev accu, []
            in
              begin
                match read_until_gt l with
                  | b, tl ->
                      if (try ignore(read_until_lt b); false with Premature_ending -> true) then (* there must not be any '<' in b *)
                        loop [Word(Printf.sprintf "<%s%s%s>" tagname (string_of_t x) (string_of_tl b))] 0 tl
                      else
                        raise Premature_ending
              end
          in
            begin
              match try Some(read_html()) with Premature_ending -> None with
                | Some (html, tl) ->
                    let r =
                      match opening with
                        | Lessthan -> r
                        | Lessthans 0 -> Text("<")::r
                        | Lessthans n -> Text(String.make (n-3) '<')::r
                        | _ -> assert false
                    in
                      main_loop (main_loop [] [] html @ r) [Greaterthan] tl
                | None ->
                    main_loop (Text(string_of_t opening^tagname)::r) [w] l
            end
              (* / end of inline HTML. *)
      | _, Newline::tl ->
          main_loop (NL::r) [Newline] tl
      | _, Newlines _::tl ->
          main_loop (NL::NL::r) [Newline] tl
      | _, Obracket::tl ->
          begin match maybe_link r previous tl with
            | r, p, l -> main_loop r p l
          end

      | _, (Exclamation|Exclamations _ as e)::(Obracket::Cbracket::Oparenthesis::tl as l) -> (* image insertion with no "alt" *)
          (* ![](/path/to/img.jpg) *)
          begin (* TODO: end of files with incomplete "images"... *)
            try
              match read_until_cparenth ~no_nl:false tl with (* new lines there seem to be allowed *)
                | b, tl ->
                    let url, tls = read_until_space b in
                    let title, should_be_empty_list = read_until_dq (snd (read_until_dq tls)) in
                    let r =
                      match e with
                        | Exclamations 0 -> Text "!" :: r
                        | Exclamations n -> Text(String.make (n+1) '!') :: r
                        | _ -> r
                    in
                    let r = Img("", string_of_tl url, string_of_tl title) :: r in
                      main_loop r [Cparenthesis] tl
            with NL_exception -> main_loop (Text(string_of_t e)::r) [Exclamation] l
          end
      | _, (Exclamation|Exclamations _ as e)::(Obracket::tl as l) -> (* image insertion with "alt" *)
          (* ![Alt text](/path/to/img.jpg "Optional title") *)
          (* TODO: end of files with incomplete "images"... *)
          begin match read_until_cbracket tl with
            | alt, Oparenthesis::ntl ->
                begin
                  try
                    let alt = string_of_tl alt in
                    let path_title, rest = read_until_cparenth ~no_nl:false ntl in
                    let path, title = read_until_space ~no_nl:true path_title in
                    let title, nothing = read_until_dq (snd(read_until_dq title)) in
                    let () = if nothing <> [] then raise NL_exception in (* this exception is caught right below *)
                    let r =
                      match e with
                        | Exclamations 0 -> Text "!" :: r
                        | Exclamations n -> Text(String.make (n+1) '!') :: r
                        | _ -> r
                    in
                    let r = Img(alt, string_of_tl path, string_of_tl title) :: r in
                      main_loop r [Cparenthesis] rest
                  with NL_exception -> (* if NL_exception was raised, then fall back to "text" *)
                    (* Below: call with (string_of_t e) even if e is (Exclamations _) because if it failed to 
                       parse an image tag, then it won't succeed if given another chance. However Obracket could 
                       announce something else, such as a link, so we have to go through it again. *)
                    main_loop (Text(string_of_t e)::r) [Exclamation] l
                end
            | _ -> main_loop (Text(string_of_t e)::r) [Exclamation] l
          end
      | _,
          ((At|Ats _|Bar|Bars _|Caret
           |Carets _|Cbrace|Cbraces _|Colon|Colons _|Comma|Commas _|Cparenthesis|Cparenthesiss _
           |Cbracket|Cbrackets _|Dollar|Dollars _|Dot|Dots _|Doublequote|Doublequotes _
           |Exclamation|Exclamations _|Equal|Equals _
           |Minus|Minuss _|Number _|Obrace
           |Obraces _|Oparenthesis|Oparenthesiss _|Obrackets _|Percent
           |Percents _|Plus|Pluss _|Question|Questions _|Quote|Quotes _|Return|Returns _
           |Semicolon|Semicolons _|Slash|Slashs _|Stars _ |Tab|Tabs _|Tilde|Tildes _
           |Underscores _
           |Lessthan|Lessthans _|Greaterthan|Greaterthans _) as t)::tl
          ->
          main_loop (Text(string_of_t t)::r) [t] tl

  and maybe_link r p l =
    let rec read_title name href res = function
      | Doublequote::(Cparenthesis as t)::tl ->
          let title = string_of_tl (List.rev res) in
            Url(href, name, title)::r, [t], tl
      | Doublequote::Cparenthesiss 0::tl ->
          let title = string_of_tl (List.rev res) in
            Url(href, name, title)::r, [Cparenthesis], Cparenthesis::tl
      | Doublequote::Cparenthesiss n::tl ->
          let title = string_of_tl (List.rev res) in
            Url(href, name, title)::r, [Cparenthesis], Cparenthesiss(n-1)::tl
      | []
      | (Newline|Newlines _)::_ as l ->
          r, p, l
      | e::tl ->
          read_title name href (e::res) tl
    in
    let rec read_url name res = function
      | Cparenthesis as t::tl ->
          let href = string_of_tl (List.rev res) in
            Url(href, name, "")::r, [t], tl
      | Cparenthesiss 0::tl ->
          let href = string_of_tl (List.rev res) in
            Url(href, name, "")::r, [Cparenthesis], Cparenthesis::tl
      | Cparenthesiss n::tl ->
          let href = string_of_tl (List.rev res) in
            Url(href, name, "")::r, [Cparenthesis], Cparenthesiss(n-1)::tl
      | (Space|Spaces _)::Doublequote::tl ->
          let href = string_of_tl (List.rev res) in
            read_title name href [] tl
      | []
      | (Newline|Newlines _)::_ as l ->
          r, p, l
      | e::tl ->
          read_url name (e::res) tl
    in
    let rec read_name res = function
      | Cbracket::Oparenthesis::tl ->
          read_url (string_of_tl (List.rev res)) [] tl
      | Cbracket::_
      | []
      | (Newline|Newlines _)::_ -> (* failed to read a MD-link *)
          r, p, l
      | e::tl ->
          read_name (e::res) tl
    in
      read_name [] l

  and read_title n (r:md) (p:tag Md_lexer.t list) (l:tag Md_lexer.t list) =
    if true then (* a behaviour closer to github *)
      begin
        let title, rest =
          let rec loop accu = function
            | ((Hash|Hashs _)::((Newline|Newlines _)::_ as l))
            | ((Hash|Hashs _)::(Space|Spaces _)::((Newline|Newlines _)::_ as l))
            | (((Newline|Newlines _)::_) as l) ->
                rev_main_loop [] [] (List.rev accu), l
            | (Hash|Hashs _ as x)::tl ->
                loop (Word(string_of_t x)::accu) tl
            | x::tl ->
                loop (x::accu) tl
            | [] ->
                rev_main_loop [] [] (List.rev accu), []
          in
            loop [] l
        in
          match n with
            | 1 -> H1 title :: r, [Newline], rest
            | 2 -> H2 title :: r, [Newline], rest
            | 3 -> H3 title :: r, [Newline], rest
            | 4 -> H4 title :: r, [Newline], rest
            | 5 -> H5 title :: r, [Newline], rest
            | 6 -> H6 title :: r, [Newline], rest
            | _ -> assert false
      end
    else (* a behaviour closer to pandoc *)
      begin
        let rec loop accu l =
          match l with
            | (Paragraph _ | H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Html _ | Url _ | Br | Hr | Code _ | Ol _ | Ul _ | NL )::_ ->
                List.rev accu, l
            | x::tl -> loop (x::accu) tl
            | [] -> List.rev accu, []
        in
        let title, rest = loop [] (rev_main_loop [] [] l) in
          match n with
            | 1 -> List.rev (H1 title :: rest) @ r, [], []
            | 2 -> List.rev (H2 title :: rest) @ r, [], []
            | 3 -> List.rev (H3 title :: rest) @ r, [], []
            | 4 -> List.rev (H4 title :: rest) @ r, [], []
            | 5 -> List.rev (H5 title :: rest) @ r, [], []
            | 6 -> List.rev (H6 title :: rest) @ r, [], []
            | _ -> assert false
      end

  and rev_main_loop (r: md) (previous:tag Md_lexer.t list) (lexemes:tag Md_lexer.t list) =
    List.rev (main_loop r previous lexemes)

  (** code that starts with one or several backquote(s) *)
  and bcode (r:md) (p:tag Md_lexer.t list) (l:tag Md_lexer.t list) : md * tag Md_lexer.t list * tag Md_lexer.t list =
    let e, tl = match l with ((Backquote|Backquotes _) as e)::tl -> e, tl | _ -> (* bcode is wrongly called *) assert false in
    let rec code_block accu = function
      | [] ->
          List.rev accu, []
      | Backquote::tl ->
          if e = Backquote then
            List.rev accu, tl
          else
            code_block (Backquote::accu) tl
      | (Backquotes n as b)::tl ->
          if e = b then
            List.rev accu, tl
          else
            code_block (b::accu) tl
      | e::tl ->
          code_block (e::accu) tl
    in
    let cb, l = code_block [] tl in
      (Code(string_of_tl cb)::r), [Backquote], l

  and icode (r:md) (p:tag Md_lexer.t list) (l:tag Md_lexer.t list) : md * tag Md_lexer.t list * tag Md_lexer.t list =
    (** indented code:
        returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
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


  (*********************************************************************************)
  (** new_list: returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
  and new_list (r:md) (p:tag Md_lexer.t list) (l:tag Md_lexer.t list) : (md * tag Md_lexer.t list * tag Md_lexer.t list) =
    if debug then Printf.eprintf "new_list p=(%s) l=(%s)\n%!" (destring_of_tl p) (destring_of_tl l);
    begin
      let list_hd e = match e with hd::_ -> hd | _ -> assert false in
      let rec loop (ordered:bool) (result:(bool*int list*tag Md_lexer.t list)list) (curr_item:tag Md_lexer.t list) (indents:int list) (lexemes:tag Md_lexer.t list) =
        let er = if debug then List.fold_left (fun r (o,il,e) -> r ^ Printf.sprintf "(%b," o ^ destring_of_tl e ^ ")") "" result else "" in
          if debug then
            begin
              Printf.eprintf "new_list>>loop er=(%s) curr_item=(%s)\n%!" er (destring_of_tl curr_item);
            end;
          match lexemes with
              (* Boolean is true if ordered, false otherwise. *)
              (* first loop: return the list of (indentation level * item) *)
            | Newline :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 1;
                if curr_item = [] then
                  loop ordered result [] (0::indents) tl
                else
                  loop false ((false,indents,curr_item)::result) [] (0::indents) tl
            | Newline :: (Number _) :: Dot :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 2;
                loop true ((true,indents,curr_item)::result) [] (0::indents) tl
            | Newline :: Space :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 3;
                loop false ((false,indents,curr_item)::result) [] (1::indents) tl
            | Newline :: Space :: Number _ :: Dot :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 4;
                loop true ((true,indents,curr_item)::result) [] (1::indents) tl
            | Newline :: ((Spaces(x) :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl) as p) ->
                if debug then Printf.eprintf "#%d\n%!" 5;
                if x+2 > list_hd indents + 4 then
                  begin (* a single new line & too many spaces -> *not* a new list item. *)
                    loop ordered result curr_item indents p (* p is what follows the new line *)
                  end
                else
                  begin (* a new list item, set previous current item as a complete item *)
                    loop false ((false,indents,curr_item)::result) [] ((x+2)::indents) tl
                  end
            | Newline :: ((Spaces(x) :: Number _ :: Dot :: (Space|Spaces _) :: tl) as p) ->
                if debug then Printf.eprintf "#%d\n%!" 6;
                if x+2 > list_hd indents + 4 then
                  begin (* a single new line & too many spaces -> *not* a new list item. *)
                    loop ordered result curr_item indents p (* p is what follows the new line *)
                  end
                else
                  begin (* a new list item, set previous current item as a complete item *)
                    loop true ((true,indents,curr_item)::result) [] ((x+2)::indents) tl
                  end
            | ([] | (Newlines(_) :: _)) as l ->
                if debug then Printf.eprintf "#%d******************************\n%!" 7;
                (* if an empty line appears, then it's the end of the list(s). *)
                ((ordered,indents,curr_item)::(result:(bool*int list*tag Md_lexer.t list) list), (l: tag Md_lexer.t list))
            | (Newline :: e :: tl)  (* adding e to the current item *)
            | e :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 8;
                loop ordered result (e::curr_item) indents tl
      in
      let rec loop2 (tmp:(bool*int list*tag Md_lexer.t list) list) (curr_indent:int) (ordered:bool) (accu:li list)
          : md * (bool*int list*tag Md_lexer.t list) list =
        let er = if debug then List.fold_left (fun r (o,il,e) -> r ^ Printf.sprintf "(%b," o ^ destring_of_tl e ^ ")") "" tmp else "" in
          if debug then Printf.eprintf "new_list>>loop2\n%!";
          match tmp with
            | (o,(i::indents), item) :: tl ->
                if debug then Printf.eprintf "@338:loop2 tmp=%s\n%!" er;
                let item = List.rev item in
                  if i = curr_indent then
                    begin
                      if debug then Printf.eprintf "PLOP\n%!";
                      loop2 tl i ordered (Li(rev_main_loop [] [Space;Star] item)::accu)
                    end
                  else if i > curr_indent then (* new sub list *)
                    begin
                      if debug then Printf.eprintf "NEW SUB LIST\n%!";
                      let md, new_tl = loop2 tl i o [Li(rev_main_loop [] [Space;Star] item)] in
                        match accu with
                          | Li hd :: accu_tl ->
                              loop2 new_tl curr_indent ordered (Li(hd@md) :: accu_tl)
                          | [] ->
                              if curr_indent = -1 then
                                md, new_tl
                              else
                                begin
                                  loop2 new_tl curr_indent ordered [Li(md)]
                                end
                    end
                  else (* i < curr_indent *)
                    let accu = List.rev accu in [if ordered then Ol accu else Ul accu], tmp
            | [(_,[],[])] | [] ->
                if debug then Printf.eprintf "FOO\n%!";
                if accu = [] then
                  [], []
                else
                  let accu = List.rev accu in [if ordered then Ol accu else Ul accu], []
            | (o,[], item) :: tl ->
                if debug then Printf.eprintf "@386:loop2 tmp=(%b,[],%s)::(%n)\n%!" o ((destring_of_tl item)) (List.length tl);
                loop2 ((o,[0], item) :: tl) curr_indent ordered accu

      in
      let (tmp_r: (bool*int list*tag Md_lexer.t list) list), (new_l:tag Md_lexer.t list) = loop true [] [] [] l in
      let () =
        if debug then
          begin
            let p =
              List.fold_left
                (fun r (o,indents,item) ->
                   Printf.sprintf "%s(%b,#%d,%s)::" r o (List.length indents) (destring_of_tl item))
                ""
                (List.rev tmp_r)
            in
              Printf.eprintf "tmp_r=%s[] new_l=%s\n%!" (p) ("")
          end
      in
      let (e:md), (x:(bool*int list*tag Md_lexer.t list) list) = loop2 (List.rev tmp_r) (-1) false [] in
        (fix_lists e @ r), [], new_l
    end


  (** spaces: returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
  and spaces n r p l =
    let spaces n r previous l =
      assert (n > 0);
      match n, previous, l with (* NOT a recursive function *)
        | (1|2|3), ([]|[(Newline|Newlines _)]), (Star|Minus|Plus)::(Space|Spaces _)::tl  (* unordered list *)
        | (1|2|3), ([]|[(Newline|Newlines _)]), (Number _)::Dot::(Space|Spaces _)::tl -> (* ordered list *)
            begin
              (new_list r [] (Newline::make_space n::l))
            end
        | (1|2|3), ([]|[(Newlines _)]), t::tl ->
            Text (" ")::r, p, l
        | (1|2|3), ([]|[(Newlines _)]), [] ->
            r, p, []
        | _, ([]|[(Newlines _)]), _ -> (* n>=4, indented code *)
            (icode r previous (make_space n :: l))
        | 1, _, _ ->
            (Text " "::r), [Space], l
        | n, _, _ -> assert (n>1);
            (Text (String.make n ' ')::r), [Spaces (n-2)], l
    in
      spaces n r p l (* NOT a recursive call *)


  in
    ( (* This is temporary, it's just to verify type inference with `ocamlc -i' *)
      xspaces := Some spaces;
      xnew_list := Some new_list;
      xicode := Some icode;
      xmain_loop := Some main_loop;
    );
    rev_main_loop [] [] lexemes


let parse lexemes =
  main_parse (tag_setext lexemes)

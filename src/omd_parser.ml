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

open Omd_backend
open Omd_lexer

type tag = Maybe_h1 | Maybe_h2 | Md of Omd_backend.md

type tmp_list = element list
and element   = Item of tag Omd_lexer.t list


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

let unindent n lexemes =
  let rec fix n p = function (* reproduce the property that twice the same token can't happen *)
    | x::tl ->
        if x = p then
          fix (n+1) p tl
        else if n = 0 then
          assert false
        else if n = 1 then
          p::fix 1 x tl
        else
          (Omd_lexer.lex(let b = Buffer.create n in for i = 1 to n do Buffer.add_string b(string_of_t p) done; Buffer.contents b))
          @ fix 1 x tl
    | [] -> []
  in
  let rec loop accu cl = function
    | Newline::Space::tl as l ->
        if n = 1 then
          loop (Newline::cl@accu) [] tl
        else
          List.rev (cl@accu), l
    | Newline::Spaces(0)::tl as l ->
        if n = 1 then
          loop (Newline::cl@accu) [Space] tl
        else if n = 2 then
          loop (Newline::cl@accu) [] tl
        else
          List.rev (cl@accu), l
    | Newline::Spaces(s)::tl as l ->
        if s+2 = n then
          loop (Newline::cl@accu) [] tl
        else if s+2 > n then
          loop (Newline::cl@accu) (Omd_lexer.lex(String.make (s+2-n) ' ')) tl
        else
          List.rev (cl@accu), l
    | Newlines(_)::_ as l ->
        List.rev (cl@accu), l
    | Newline::_ as l ->
        List.rev (cl@accu), l
    | e::tl ->
        loop accu (e::cl) tl
    | [] as l -> 
        List.rev (cl@accu), l
  in 
  match loop [] [] lexemes with
    | [], right -> [], right
    | (e::tl), right -> fix 1 e tl, right
  



let rec is_blank = function
  | (Space | Spaces _ | Newline | Newlines _) :: tl ->
      is_blank tl
  | [] -> true
  | _ -> false

(** [emph_or_bold (n:int) (r:md list) (l:Omd_lexer.t list)]
    returns [] if not (emph and/or bold),
    else returns the contents intended to be formatted,
    along with the rest of the stream that hasn't been processed. *)
let emph_or_bold (n:int) (l:tag Omd_lexer.t list) : (tag Omd_lexer.t list * tag Omd_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:tag Omd_lexer.t list) = function
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

(** [uemph_or_bold (n:int) (r:md list) (l:tag Omd_lexer.t list)]
    returns [] if not (emph and/or bold),
    else returns the contents intended to be formatted,
    along with the rest of the stream that hasn't been processed. *)
let uemph_or_bold (n:int) (l:tag Omd_lexer.t list) : (tag Omd_lexer.t list * tag Omd_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:tag Omd_lexer.t list) = function
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


let gh_uemph_or_bold (n:int) (l:tag Omd_lexer.t list) : (tag Omd_lexer.t list * tag Omd_lexer.t list) =
  assert (n>0 && n<4);
  let rec loop (result:tag Omd_lexer.t list) = function
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


(* The few following lines are there for development purpose only. *)
let xspaces = ref None
let xnew_list = ref None
let xicode = ref None
let xmain_loop = ref None

(* [eat f l] removes elements from [l] until [f] meets an element 
   for which it returns false. If [l] is empty, then returns [l]. *)
let rec eat f = function
  | [] -> []
  | e::tl -> if f e then eat f tl else e::tl

let rec mem f = function
  | [] -> false
  | e::tl -> f e || mem f tl  

let split_norev f =
  let rec loop r = function
  | [] -> r, []
  | e::tl as l -> if f e then loop (e::r) tl else r, l
  in loop []

(* [split f l] *)
let split f l =
  let r, l = split_norev f l in
    List.rev r, l

(* Let's tag the lines that *might* be titles using setext-style. 
   "might" because if they are, for instance, in a code section, 
   then they are not titles at all. *)
let tag_setext lexemes =
  let rec loop pl res = function
    | (Newline as e1)::(Equal|Equals _ as e2)::tl -> (* might be a H1. *)
        begin match split_norev (function (Space|Spaces _|Equal|Equals _) -> true|_ -> false) tl with
          | rleft, (([]|(Newline|Newlines _)::_) as right) ->
              loop [] (rleft@(e2::e1::pl@(Tag(Maybe_h1)::res))) right
          | rleft, right ->
              loop [] (rleft@(e2::e1::pl@res)) right
        end
    | (Newline as e1)::(Minus|Minuss _ as e2)::tl -> (* might be a H2. *)
        begin match split_norev (function (Space|Spaces _|Minus|Minuss _) -> true|_ -> false) tl with
          | rleft, (([]|(Newline|Newlines _)::_) as right) ->
              loop [] (rleft@(e2::e1::pl@(Tag(Maybe_h2)::res))) right
          | rleft, right ->
              loop [] (rleft@(e2::e1::pl@res)) right
        end
    | (Newlines _ as e1)::tl ->
        loop [] (e1::pl@res) tl
    | e::tl ->
        loop (e::pl) res tl
    | [] ->
        pl@res
  in
    List.rev (loop [] [] lexemes)

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
    | ((Newlines _|Newline)::tl) | ([] as tl) ->
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
  | Ul[] :: tl ->
      fix_lists tl
  | Ol[] :: tl ->
      fix_lists tl
  | Ul(Li(Ul(_) :: _ as l) :: l2) :: tl
  | Ul(Li(Ol(_) :: _ as l) :: l2) :: tl ->
      fix_lists [Ul(l2)] @ fix_lists l @ fix_lists tl
  | Ol(Li(Ul(_) :: _ as l) :: l2) :: tl
  | Ol(Li(Ol(_) :: _ as l) :: l2) :: tl ->
      fix_lists [Ol(l2)] @ fix_lists l @ fix_lists tl
  | Ul(l) :: tl ->
      Ul(List.map (fun (Li e) -> Li(fix_lists e)) l) :: fix_lists tl
  | Ol(l) :: tl ->
      Ol(List.map (fun (Li e) -> Li(fix_lists e)) l) :: fix_lists tl
  | Blockquote(q) :: tl ->
      Blockquote(fix_lists q) :: fix_lists tl
  | Img _ as i :: tl ->
      i :: fix_lists tl
  | Paragraph p ::  tl ->
      Paragraph (fix_lists p) :: fix_lists tl
  | Text _ as e :: tl ->
      e::fix_lists tl
  | Emph e :: tl ->
      Emph(fix_lists e)::fix_lists tl
  | Bold e :: tl ->
      Bold(fix_lists e)::fix_lists tl
  | (Code _ |Code_block _ | Br | Hr | Ref _ | Img_ref _ | Url _ | Html _ | Html_block _ as e) :: tl ->
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

let read_until_q ?(no_nl=false) l =
  let rec loop accu = function
    | Quote :: tl -> (List.rev (accu)), tl
    | Quotes 0 :: tl -> (List.rev (accu)), Quote::tl
    | Quotes n :: tl -> (List.rev (accu)), Quotes(n-1)::tl
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

let read_until_obracket ?(no_nl=false) l =
  let rec loop accu = function
    | Obracket :: tl -> (List.rev (accu)), tl
    | Obrackets 0 :: tl -> (List.rev (accu)), Obracket::tl
    | Obrackets n :: tl -> (List.rev (accu)), Obrackets(n-1)::tl
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
  let rc = new Omd_backend.ref_container in
  let rec main_loop (r:md) (previous:tag Omd_lexer.t list) (lexemes:tag Omd_lexer.t list) =
    if debug then Printf.eprintf "main_loop r=%s p=(%s) l=(%s)\n%!" (Omd_backend.sexpr_of_md (List.rev r)) (destring_of_tl previous) (destring_of_tl lexemes);
    match previous, lexemes with
        (* no more to process *)
      | _, [] -> (* return the result (/!\ it has to be reversed as some point) *)
          r

      (* previously processed *)
      | _, Tag(Md(md))::tl -> (* md should be in reverse, such as r *)
          main_loop (md@r) [] tl

      (* email-style quoting *)
      | ([]|[Newline|Newlines _]), Greaterthan::(Space|Spaces _)::_ ->
          begin 
            let r, p, l = emailstyle_quoting r previous (Newline::lexemes)
            in main_loop r p l
          end

      (* email-style quoting, with lines starting with spaces! *)
      | ([]|[Newline|Newlines _]), (Space|Spaces(0|1) as s)::Greaterthan::(Space|Spaces _)::_ ->
          begin
            let new_r, p, rest =
              let foo, rest = unindent (fst(length s)) (Newline::lexemes) in
                match emailstyle_quoting [] previous (Newline::(foo)) with
                  | new_r, p, [] -> new_r, p, rest
                  | _ -> assert false
            in main_loop (new_r@r) [Newline] rest
          end

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
                begin match new_list false r [] (Newline::lexemes) with
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

      (* At least 4 spaces, so it can only be code. *)
      | ([]|[Newline|Newlines _]), (Spaces n)::tl when n>=2 ->
          let r, p, l = icode r [Newline] lexemes in
            main_loop r p l

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
      | _, (Underscores((0|1) as n) as t) :: tl -> (* 2 or 3 "orphan" underscores, or emph/bold *)
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

      (* enumerated lists *)
      | ([]|[Newline|Newlines _]), (Number _) :: Dot :: (Space|Spaces _) :: tl ->
          if debug then Printf.eprintf "++++++++++++++++++++++++++++++\n(%s)\n%!" (String.escaped(string_of_tl lexemes));
          begin match new_list true r [] (Newline::lexemes) with
            | md, new_p, new_l ->
                main_loop (md) new_p new_l
          end

      (* stars *)
      | ([]|[(Newline|Newlines _)]), Star :: (Space|Spaces _) :: _ -> (* maybe hr or new list *)
          begin match hr_s lexemes with
            | Some l ->
                main_loop (Hr::r) [Newline] l
            | None ->
                begin match new_list false r [] (Newline::lexemes) with
                  | md, new_p, new_l -> main_loop (md) new_p new_l
                end
          end
      | ([]|[(Newline|Newlines _)]), Stars _ :: _ when (not (hr_s lexemes = None)) -> (* hr *)
          begin match hr_s lexemes with
            | Some l -> main_loop (Hr::r) [Newline] l
            | None -> assert false
          end
      | ([]|[(Newline|Newlines _)]), (Star as t) :: tl -> (* maybe hr *)
          begin match hr_s lexemes with
            | Some l ->
                main_loop (Hr::r) [Newline] l
            | None ->
                begin match emph_or_bold 1 tl with
                  | [], _      -> main_loop (Text(string_of_t t) :: r) [t] tl
                  | x , new_tl -> main_loop (Emph(rev_main_loop [] [t] x) :: r) [t] new_tl                  
                end
                  
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
      | _, (Backslashs 0 as t) :: tl -> (* \\\\... *)
          main_loop (Text ("\\") :: r) [t] tl
      | _, (Backslashs n as t) :: tl -> (* \\\\... *)
          if n mod 2 = 0 then
            main_loop (Text (String.make ((n-2)/2) '\\') :: r) [t] tl
          else
            main_loop (Text (String.make ((n-2)/2) '\\') :: r) [t] (Backslash :: tl)
      | _, Backslash::[] ->
          main_loop (Text "\\" :: r) [] []
      | _, Backslash::tl ->
          main_loop (Text "\\" :: r) [Backslash] tl
            
      (* < *)
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

      (* Word(w) *)
      | _, Word w::tl ->
          main_loop (Text w :: r) [Word w] tl

      (* newline at the end *)
      | _, [Newline] ->
          NL::r

      (* named html entity *)
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

      (* digit-coded html entity *)
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

      (* Ampersand *)
      | _, Ampersand::tl ->
          main_loop (Html("&amp;")::r) [Ampersand] tl

      (* 2 Ampersands *)
      | _, Ampersands(0)::tl ->
          main_loop (Html("&amp;")::r) [] (Ampersand::tl)

      (* Several Ampersands (more than 2) *)
      | _, Ampersands(n)::tl ->
          main_loop (Html("&amp;")::r) [] (Ampersands(n-1)::tl)

      (* backquotes *)
      | _, (Backquote|Backquotes _)::_ ->
          begin match bcode r previous lexemes with
            | r, p, l -> main_loop r p l
          end

      (* HTML *)
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
                  | html, tl -> main_loop (Html_block(string_of_tl html)::r) [Greaterthan] tl
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


      (* line breaks *)
      | _, Newline::tl ->
          main_loop (NL::r) [Newline] tl
      | _, Newlines _::tl ->
          main_loop (NL::NL::r) [Newline] tl

      (* [ *)
      | _, Obracket::tl ->
          begin match maybe_link r previous tl with
            | Some(r, p, l) -> main_loop r p l
            | None ->
                match maybe_reference r previous tl with
                  | Some(r, p, l) -> main_loop r p l
                  | None -> main_loop (Text("[")::r) [Obracket] tl
          end
            
      (* img *)
      | _, (Exclamation|Exclamations _ as e)::(Obracket::Cbracket::Oparenthesis::tl as l) -> (* image insertion with no "alt" *)
          (* ![](/path/to/img.jpg) *)
          begin
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

      (* img ref *)
      | _, (Exclamation|Exclamations _ as e)::(Obracket::Cbracket::Obracket::tl as l) -> (* ref image insertion with no "alt" *)
          (* ![][ref] *)
          begin
            try
              match read_until_cbracket ~no_nl:true tl with
                | id, tl ->
                    let r =
                      match e with
                        | Exclamations 0 -> Text "!" :: r
                        | Exclamations n -> Text(String.make (n+1) '!') :: r
                        | _ -> r
                    in
                    let r = Img_ref(rc, string_of_tl id, "") :: r in
                      main_loop r [Cbracket] tl
            with NL_exception -> main_loop (Text(string_of_t e)::r) [Exclamation] l
          end


      (* img *)
      | _, (Exclamation|Exclamations _ as e)::(Obracket::tl as l) -> (* image insertion with "alt" *)
          (* ![Alt text](/path/to/img.jpg "Optional title") *)
          begin
            try
              match read_until_cbracket tl with
                | alt, Oparenthesis::ntl ->
                    begin
                      try
                        let alt = string_of_tl alt in
                        let path_title, rest = read_until_cparenth ~no_nl:false ntl in
                        let path, title = try read_until_space ~no_nl:true path_title with Premature_ending -> path_title, [] in
                        let title, nothing = if title <> [] then read_until_dq (snd(read_until_dq title)) else [], [] in
                        let () = if nothing <> [] then raise NL_exception else () in (* this exception is caught right below *)
                        let r =
                          match e with
                            | Exclamations 0 -> Text "!" :: r
                            | Exclamations n -> Text(String.make (n+1) '!') :: r
                            | _ -> r
                        in
                        let r = Img(alt, string_of_tl path, string_of_tl title) :: r in
                          main_loop r [Cparenthesis] rest
                      with
                        | NL_exception -> (* if NL_exception was raised, then fall back to "text" *)
                            (* Below: call with (string_of_t e) even if e is (Exclamations _) because if it failed to 
                               parse an image tag, then it won't succeed if given another chance. However Obracket could 
                               announce something else, such as a link, so we have to go through it again. *)
                            main_loop (Text(string_of_t e)::r) [Exclamation] l
                        | Premature_ending ->
                                main_loop (Text(string_of_t e)::r) [Exclamation] l
                    end
                | alt, Obracket::Word(id)::Cbracket::ntl 
                | alt, Obracket::(Space|Spaces _)::Word(id)::Cbracket::ntl 
                | alt, Obracket::(Space|Spaces _)::Word(id)::(Space|Spaces _)::Cbracket::ntl
                | alt, Obracket::Word(id)::(Space|Spaces _)::Cbracket::ntl ->
                    main_loop (Img_ref(rc, id, string_of_tl alt)::r) [Cbracket] ntl
                | alt, Obracket::((Newline|Space|Spaces _|Word _|Number _)::_ as ntl) ->
                    begin
                      try
                        match read_until_cbracket ~no_nl:false ntl with
                          | [], rest -> raise Premature_ending
                          | id, rest -> main_loop (Img_ref(rc, string_of_tl id, string_of_tl alt)::r) [Cbracket] rest
                      with Premature_ending | NL_exception -> main_loop (Text(string_of_t e)::r) [Exclamation] l
                    end
                | _ -> main_loop (Text(string_of_t e)::r) [Exclamation] l
            with 
              | Premature_ending ->
                  main_loop (Text(string_of_t e)::r) [Exclamation] l (* todo:  *)
          end

      | _,
          ((At|Bar|Caret|Cbrace|Colon|Comma|Cparenthesis|Cbracket|Dollar|Dot|Doublequote
          |Exclamation|Equal|Minus|Obrace|Oparenthesis|Percent|Plus|Question|Quote|Return
          |Semicolon|Slash|Tab|Tilde|Lessthan|Greaterthan) as t)::tl
          ->
          main_loop (Text(string_of_t t)::r) [t] tl

      | _, (Number(_) as t) :: tl -> main_loop (Text(string_of_t t)::r) [t] tl

      (* generated part: *)
      | _, Ats(0) :: tl -> main_loop (Text(string_of_t(At))::r) [At] (At::tl)
      | _, Ats(n) :: tl -> main_loop (Text(string_of_t(At))::r) [At] (Ats(n-1)::tl)
      | _, Bars(0) :: tl -> main_loop (Text(string_of_t(Bar))::r) [Bar] (Bar::tl)
      | _, Bars(n) :: tl -> main_loop (Text(string_of_t(Bar))::r) [Bar] (Bars(n-1)::tl)
      | _, Carets(0) :: tl -> main_loop (Text(string_of_t(Caret))::r) [Caret] (Caret::tl)
      | _, Carets(n) :: tl -> main_loop (Text(string_of_t(Caret))::r) [Caret] (Carets(n-1)::tl)
      | _, Cbraces(0) :: tl -> main_loop (Text(string_of_t(Cbrace))::r) [Cbrace] (Cbrace::tl)
      | _, Cbraces(n) :: tl -> main_loop (Text(string_of_t(Cbrace))::r) [Cbrace] (Cbraces(n-1)::tl)
      | _, Colons(0) :: tl -> main_loop (Text(string_of_t(Colon))::r) [Colon] (Colon::tl)
      | _, Colons(n) :: tl -> main_loop (Text(string_of_t(Colon))::r) [Colon] (Colons(n-1)::tl)
      | _, Commas(0) :: tl -> main_loop (Text(string_of_t(Comma))::r) [Comma] (Comma::tl)
      | _, Commas(n) :: tl -> main_loop (Text(string_of_t(Comma))::r) [Comma] (Commas(n-1)::tl)
      | _, Cparenthesiss(0) :: tl -> main_loop (Text(string_of_t(Cparenthesis))::r) [Cparenthesis] (Cparenthesis::tl)
      | _, Cparenthesiss(n) :: tl -> main_loop (Text(string_of_t(Cparenthesis))::r) [Cparenthesis] (Cparenthesiss(n-1)::tl)
      | _, Cbrackets(0) :: tl -> main_loop (Text(string_of_t(Cbracket))::r) [Cbracket] (Cbracket::tl)
      | _, Cbrackets(n) :: tl -> main_loop (Text(string_of_t(Cbracket))::r) [Cbracket] (Cbrackets(n-1)::tl)
      | _, Dollars(0) :: tl -> main_loop (Text(string_of_t(Dollar))::r) [Dollar] (Dollar::tl)
      | _, Dollars(n) :: tl -> main_loop (Text(string_of_t(Dollar))::r) [Dollar] (Dollars(n-1)::tl)
      | _, Dots(0) :: tl -> main_loop (Text(string_of_t(Dot))::r) [Dot] (Dot::tl)
      | _, Dots(n) :: tl -> main_loop (Text(string_of_t(Dot))::r) [Dot] (Dots(n-1)::tl)
      | _, Doublequotes(0) :: tl -> main_loop (Text(string_of_t(Doublequote))::r) [Doublequote] (Doublequote::tl)
      | _, Doublequotes(n) :: tl -> main_loop (Text(string_of_t(Doublequote))::r) [Doublequote] (Doublequotes(n-1)::tl)
      | _, Exclamations(0) :: tl -> main_loop (Text(string_of_t(Exclamation))::r) [Exclamation] (Exclamation::tl)
      | _, Exclamations(n) :: tl -> main_loop (Text(string_of_t(Exclamation))::r) [Exclamation] (Exclamations(n-1)::tl)
      | _, Equals(0) :: tl -> main_loop (Text(string_of_t(Equal))::r) [Equal] (Equal::tl)
      | _, Equals(n) :: tl -> main_loop (Text(string_of_t(Equal))::r) [Equal] (Equals(n-1)::tl)
      | _, Minuss(0) :: tl -> main_loop (Text(string_of_t(Minus))::r) [Minus] (Minus::tl)
      | _, Minuss(n) :: tl -> main_loop (Text(string_of_t(Minus))::r) [Minus] (Minuss(n-1)::tl)
      | _, Obraces(0) :: tl -> main_loop (Text(string_of_t(Obrace))::r) [Obrace] (Obrace::tl)
      | _, Obraces(n) :: tl -> main_loop (Text(string_of_t(Obrace))::r) [Obrace] (Obraces(n-1)::tl)
      | _, Obrackets(0) :: tl -> main_loop (Text(string_of_t(Obracket))::r) [Obracket] (Obracket::tl)
      | _, Obrackets(n) :: tl -> main_loop (Text(string_of_t(Obracket))::r) [Obracket] (Obrackets(n-1)::tl)
      | _, Percents(0) :: tl -> main_loop (Text(string_of_t(Percent))::r) [Percent] (Percent::tl)
      | _, Percents(n) :: tl -> main_loop (Text(string_of_t(Percent))::r) [Percent] (Percents(n-1)::tl)
      | _, Pluss(0) :: tl -> main_loop (Text(string_of_t(Plus))::r) [Plus] (Plus::tl)
      | _, Pluss(n) :: tl -> main_loop (Text(string_of_t(Plus))::r) [Plus] (Pluss(n-1)::tl)
      | _, Questions(0) :: tl -> main_loop (Text(string_of_t(Question))::r) [Question] (Question::tl)
      | _, Questions(n) :: tl -> main_loop (Text(string_of_t(Question))::r) [Question] (Questions(n-1)::tl)
      | _, Quotes(0) :: tl -> main_loop (Text(string_of_t(Quote))::r) [Quote] (Quote::tl)
      | _, Quotes(n) :: tl -> main_loop (Text(string_of_t(Quote))::r) [Quote] (Quotes(n-1)::tl)
      | _, Returns(0) :: tl -> main_loop (Text(string_of_t(Return))::r) [Return] (Return::tl)
      | _, Returns(n) :: tl -> main_loop (Text(string_of_t(Return))::r) [Return] (Returns(n-1)::tl)
      | _, Semicolons(0) :: tl -> main_loop (Text(string_of_t(Semicolon))::r) [Semicolon] (Semicolon::tl)
      | _, Semicolons(n) :: tl -> main_loop (Text(string_of_t(Semicolon))::r) [Semicolon] (Semicolons(n-1)::tl)
      | _, Stars(n) :: tl -> main_loop (Text(string_of_t(Star))::r) [Star] (Stars(n-1)::tl)
      | _, Tabs(0) :: tl -> main_loop (Text(string_of_t(Tab))::r) [Tab] (Tab::tl)
      | _, Tabs(n) :: tl -> main_loop (Text(string_of_t(Tab))::r) [Tab] (Tabs(n-1)::tl)
      | _, Tildes(0) :: tl -> main_loop (Text(string_of_t(Tilde))::r) [Tilde] (Tilde::tl)
      | _, Tildes(n) :: tl -> main_loop (Text(string_of_t(Tilde))::r) [Tilde] (Tildes(n-1)::tl)
      | _, Underscores(n) :: tl -> main_loop (Text(string_of_t(Underscore))::r) [Underscore] (Underscores(n-1)::tl)
      | _, Lessthans(0) :: tl -> main_loop (Text(string_of_t(Lessthan))::r) [Lessthan] (Lessthan::tl)
      | _, Lessthans(n) :: tl -> main_loop (Text(string_of_t(Lessthan))::r) [Lessthan] (Lessthans(n-1)::tl)
      | _, Greaterthans(0) :: tl -> main_loop (Text(string_of_t(Greaterthan))::r) [Greaterthan] (Greaterthan::tl)
      | _, Greaterthans(n) :: tl -> main_loop (Text(string_of_t(Greaterthan))::r) [Greaterthan] (Greaterthans(n-1)::tl)
      | _, Oparenthesiss(0) :: tl -> main_loop (Text(string_of_t(Oparenthesis))::r) [Oparenthesis] (Oparenthesis::tl)
      | _, Oparenthesiss(n) :: tl -> main_loop (Text(string_of_t(Oparenthesis))::r) [Oparenthesis] (Oparenthesiss(n-1)::tl)
      | _, Slashs(0) :: tl -> main_loop (Text(string_of_t(Slash))::r) [Slash] (Slash::tl)
      | _, Slashs(n) :: tl -> main_loop (Text(string_of_t(Slash))::r) [Slash] (Slashs(n-1)::tl)
          (* /generated part *)

  and emailstyle_quoting r previous lexemes =
    let rec loop (block:tag Omd_lexer.t list) (cl:tag Omd_lexer.t list) = function
      | Newline::Greaterthan::(Newline::_ as tl) -> loop (Newline::cl@block) [] tl
      | Newline::Greaterthan::Space::tl -> loop (Newline::cl@block) [] tl
      | Newline::Greaterthan::Spaces 0::tl -> loop (Newline::cl@block) [Space] tl
      | Newline::Greaterthan::Spaces n::tl -> loop (Newline::cl@block) [Spaces(n-1)] tl
          (* | Newline::tl -> loop block (Newline::cl) tl *)
      | (Newlines _::_ as l) | ([] as l) -> List.rev (cl@block), l
      | e::tl -> loop block (e::cl) tl
    in
      match loop [] [] lexemes with
        | block, tl ->
            if debug then Printf.eprintf "##############################\n%s\n##############################\n%!" (string_of_tl block);
            (Blockquote(rev_main_loop [] [] block)::r), [Newline], tl

  (* maybe a reference *)
  and maybe_reference r p l = (* this function is called when we know it's not a link although it started with a '[' *)
    (* So it could be a reference or a link definition. *)
    let rec maybe_ref l =
      let text, remains = read_until_cbracket l in
      let () = if try ignore(read_until_obracket text); true with Premature_ending -> false then raise Premature_ending in
      let blank, remains = read_until_obracket remains in
      let () = if eat (function (Space|Spaces _|Newline|Newlines _) -> true| _ -> false) blank <> [] then raise Premature_ending in
        match read_until_cbracket remains with
          | [], remains ->
              let id = string_of_tl text in (* implicit anchor *)
              Some(((Ref(rc, id, id))::r), [Cbracket], remains)
          | id, remains ->
              Some(((Ref(rc, string_of_tl id, string_of_tl text))::r), [Cbracket], remains)
    in
    let rec maybe_def l =
      match read_until_cbracket l with
        | _, [] -> None
        | id, (Colon::(Space|Spaces _)::remains)
        | id, (Colon::remains) ->
            begin
              let url, remains =
                split (function (Space|Spaces _|Newline|Newlines _) -> false|_ -> true) remains
              in
              let title, remains =
                match eat (function (Space|Spaces _|Newline|Newlines _) -> true| _ -> false) remains with
                  | Doublequotes(0)::tl -> [], tl
                  | Doublequote::tl -> read_until_dq tl
                  | Quotes(0)::tl -> [], tl
                  | Quote::tl -> read_until_q tl
                  | Oparenthesis::tl-> read_until_cparenth tl
                  | l -> [], l
              in
                rc#add_ref (string_of_tl id) (string_of_tl title) (string_of_tl url);
                Some(r, [Quote], remains)
            end
        | _ -> None
    in
      try maybe_ref l with Premature_ending | NL_exception -> 
        try maybe_def l with Premature_ending | NL_exception ->
          None

  (* maybe a link *)
  and maybe_link r p l =
    let rec read_title name href res = function
      | Doublequote::(Cparenthesis as t)::tl ->
          let title = string_of_tl (List.rev res) in
            Some(Url(href, name, title)::r, [t], tl)
      | Doublequote::Cparenthesiss 0::tl ->
          let title = string_of_tl (List.rev res) in
            Some(Url(href, name, title)::r, [Cparenthesis], Cparenthesis::tl)
      | Doublequote::Cparenthesiss n::tl ->
          let title = string_of_tl (List.rev res) in
            Some(Url(href, name, title)::r, [Cparenthesis], Cparenthesiss(n-1)::tl)
      | [] ->
          None
      | e::tl ->
          read_title name href (e::res) tl
    in
    let rec read_url name res = function
      | Cparenthesis as t::tl ->
          let href = string_of_tl (List.rev res) in
            Some(Url(href, name, "")::r, [t], tl)
      | Cparenthesiss 0::tl ->
          let href = string_of_tl (List.rev res) in
            Some(Url(href, name, "")::r, [Cparenthesis], Cparenthesis::tl)
      | Cparenthesiss n::tl ->
          let href = string_of_tl (List.rev res) in
            Some(Url(href, name, "")::r, [Cparenthesis], Cparenthesiss(n-1)::tl)
      | (Space|Spaces _)::Doublequote::tl ->
          let href = string_of_tl (List.rev res) in
            read_title name href [] tl
      | [] | (Newline|Newlines _)::_  ->
          None
      | e::tl ->
          read_url name (e::res) tl
    in
    let rec read_name res = function
      | Cbracket::Oparenthesis::tl ->
          read_url (string_of_tl (List.rev res)) [] tl
      | [] | Cbracket::_ -> (* failed to read a MD-link *)
          None
      | e::tl ->
          read_name (e::res) tl
    in
      read_name [] l

  (* H1, H2, H3, ... *)
  and read_title n (r:md) (p:tag Omd_lexer.t list) (l:tag Omd_lexer.t list) =
    if true then (* a behaviour closer to github *)
      begin
        let title, rest =
          let rec loop accu = function
            | ((Hash|Hashs _)::((Newline|Newlines _)::_ as l))
            | ((Hash|Hashs _)::(Space|Spaces _)::((Newline|Newlines _)::_ as l))
            | (((Newline|Newlines _)::_) as l)
            | ([] as l) ->
                rev_main_loop [] [] (List.rev accu), l
            | (Hash|Hashs _)::[] ->
                rev_main_loop [] [] (List.rev accu), []
            | (Hash|Hashs _ as x)::tl ->
                loop (Word(string_of_t x)::accu) tl
            | x::tl ->
                loop (x::accu) tl
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
            | (Paragraph _ | H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Html _ | Html_block _ | Url _ | Br | Hr | Code _ | Code_block _ | Ol _ | Ul _ | NL )::_ ->
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

  and rev_main_loop (r: md) (previous:tag Omd_lexer.t list) (lexemes:tag Omd_lexer.t list) =
    List.rev (main_loop r previous lexemes)

  (** code that starts with one or several backquote(s) *)
  and bcode (r:md) (p:tag Omd_lexer.t list) (l:tag Omd_lexer.t list) : md * tag Omd_lexer.t list * tag Omd_lexer.t list =
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
      if mem (function (Newline|Newlines _) -> true | _ -> false) cb then
        (Code_block(string_of_tl cb)::r), [Backquote], l
      else
        let clean_bcode s =
          let rec loop1 i =
            if i = String.length s then 0
            else match s.[i] with
              | '`' -> i
              | ' ' -> loop1(i+1)
              | _ -> 0
          in
          let rec loop2 i =
            if i = -1 then String.length s - 1
            else match s.[i] with
              | '`' -> i+1
              | ' ' -> loop2(i-1)
              | _ -> String.length s - 1
          in
            match loop1 0, loop2 (String.length s - 1) with
              | 0, n when n = String.length s - 1 -> s
              | i, n -> String.sub s i (n-i)
        in
          (Code(clean_bcode(string_of_tl cb))::r), [Backquote], l

  and icode (r:md) (p:tag Omd_lexer.t list) (l:tag Omd_lexer.t list) : md * tag Omd_lexer.t list * tag Omd_lexer.t list =
    (** indented code:
        returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
    let accu = Buffer.create 42 in
    let rec loop = function
      | (([]|[Newline|Newlines _]) as p), (((Space|Spaces(0|1))::_) as tl) ->  (* 1, 2 or 3 spaces. *)
          Code_block (Buffer.contents accu)::r, p, tl (* -> Return what's been found as code because it's no more code. *)
      | ([]|[Newline|Newlines _]), (Spaces(n) as t)::tl -> (* At least 4 spaces, it's still code. *)
          Buffer.add_string accu (String.make (n-2) ' ');
          loop ([t], tl)
      | ([(Newline|Newlines _)] as p), not_spaces::tl -> (* stop *)
          Code_block (Buffer.contents accu)::r, p, tl (* -> Return what's been found as code because it's no more code. *)
      | _, e::tl ->
          Buffer.add_string accu (string_of_t e); (* html entities are to be converted later! *)
          loop ([e], tl)
      | p, [] ->
          Code_block (Buffer.contents accu)::r, p, []
    in loop ([Newlines 0], l)


  (*********************************************************************************)
  (** new_list: returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
  and new_list (o:bool) (r:md) (p:tag Omd_lexer.t list) (l:tag Omd_lexer.t list) : (md * tag Omd_lexer.t list * tag Omd_lexer.t list) =
    if debug then Printf.eprintf "new_list p=(%s) l=(%s)\n%!" (destring_of_tl p) (destring_of_tl l);
    begin
      let list_hd e = match e with hd::_ -> hd | _ -> assert false in
      let rec loop (fi:bool) (ordered:bool) (result:(bool*int list*tag Omd_lexer.t list)list) (curr_item:tag Omd_lexer.t list) (indents:int list) (lexemes:tag Omd_lexer.t list) =
        (* 'fi' means first iteration *)
        let er = if debug then List.fold_left (fun r (o,il,e) -> r ^ Printf.sprintf "(%b," o ^ destring_of_tl e ^ ")") "" result else "" in
          if debug then
            begin
              Printf.eprintf "new_list>>loop er=(%s) curr_item=(%s) lexemes=%s\n%!" er (destring_of_tl curr_item) (destring_of_tl lexemes);
            end;
          match lexemes with
              (* Boolean is true if ordered, false otherwise. *)
              (* first loop: return the list of (indentation level * item) *)

            (* indent = 0 *)
            | (Newline|Newlines 0) :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 1;
                if fi then
                  loop false ordered result [] (0::indents) tl
                else
                  loop false false ((false,indents,curr_item)::result) [] (0::indents) tl
            | (Newline|Newlines 0) :: (Number _) :: Dot :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 2;
                if fi then
                  loop false ordered result [] (0::indents) tl
                else
                  loop false true ((true,indents,curr_item)::result) [] (0::indents) tl

            (* indent = 1 *)
            | (Newline|Newlines 0) :: Space :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 3;
                if fi then
                  loop false ordered result [] (1::indents) tl
                else
                  loop false false ((false,indents,curr_item)::result) [] (1::indents) tl
            | (Newline|Newlines 0) :: Space :: Number _ :: Dot :: (Space|Spaces _) :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 4;
                if fi then
                  loop false ordered result [] (1::indents) tl
                else
                  loop false true ((true,indents,curr_item)::result) [] (1::indents) tl

            (* indent >= 2 *)
            | (Newline|Newlines 0) :: ((Spaces(x) :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl) as p) ->
                if debug then Printf.eprintf "#%d\n%!" 5;
                if x+2 > list_hd indents + 4 then
                  begin (* a single new line & too many spaces -> *not* a new list item. *)
                    loop false ordered result curr_item indents p (* p is what follows the new line *)
                  end
                else
                  begin (* a new list item, set previous current item as a complete item *)
                    if fi then
                      loop false ordered result [] ((x+2)::indents) tl
                    else
                      loop false false ((false,indents,curr_item)::result) [] ((x+2)::indents) tl
                  end
            | (Newline|Newlines 0) :: ((Spaces(x) :: Number _ :: Dot :: (Space|Spaces _) :: tl) as p) ->
                if debug then Printf.eprintf "#%d\n%!" 6;
                if x+2 > list_hd indents + 4 then
                  begin (* a single new line & too many spaces -> *not* a new list item. *)
                    loop false ordered result curr_item indents p (* p is what follows the new line *)
                  end
                else
                  begin (* a new list item, set previous current item as a complete item *)
                    if fi then
                      loop false ordered result [] ((x+2)::indents) tl
                    else
                      loop false true ((true,indents,curr_item)::result) [] ((x+2)::indents) tl
                  end
            | Newlines(0) :: ((Spaces(2|3|4|5 as n)) :: Greaterthan :: (Space|Spaces _) :: tl as l) -> (* blockquote inside a list *)
                begin match unindent (n+2) (Newline::l) with
                  | block, rest ->
                      let em, _, _x = emailstyle_quoting [] [] block in
                        assert(_x = []);
                        loop false ordered result (Tag(Md(em))::curr_item) indents rest
                end
            | Newlines(0) :: (Spaces(n) :: tl as l) 
            | Newline::Newline:: (Spaces(n) :: tl as l) when (try n+2 >= List.hd indents+4 with _ -> assert false) -> (* code inside a list *)
                begin match unindent (List.hd indents+4) (Newline::l) with
                  | block, rest ->
                      loop false ordered result (Tag(Md(main_loop [] [] block))::curr_item) indents rest                  
                end

            | ((Newline|Newlines 0 as k) :: Spaces(_) :: e :: tl) -> (* adding e to the current item *)
                if debug then Printf.eprintf "#%d (%s)\n%!" 88 (destring_of_tl lexemes);
                loop false ordered result (e::Space::k::curr_item) indents tl

            | ((Newline|Newlines 0 as k) :: e :: tl) -> (* adding e to the current item *)
                if debug then Printf.eprintf "#%d (%s)\n%!" 8 (destring_of_tl lexemes);
                loop false ordered result (e::k::curr_item) indents tl

            | ([] | (Newlines(_) :: _)) ->
                if debug then Printf.eprintf "#%d******************************\n%!" 7;
                (* if an empty line appears, then it's the end of the list(s). *)
                ((ordered,indents,curr_item)::(result:(bool*int list*tag Omd_lexer.t list) list), lexemes)

            | e :: tl -> (* adding e to the current item *)
                if debug then Printf.eprintf "#%d (%s)\n%!" 9 (destring_of_tl lexemes);
                loop false ordered result (e::curr_item) indents tl
      in
      let rec loop2 (tmp:(bool*int list*tag Omd_lexer.t list) list) (curr_indent:int) (ordered:bool) (accu:li list)
          : md * (bool*int list*tag Omd_lexer.t list) list =
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
      let tmp_r, new_l =
        (* tmp_r: (bool*int list*tag Omd_lexer.t list) list) ; new_l:tag Omd_lexer.t list *)
        loop true o [] [] [] l 
      in
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
      let (e:md), (x:(bool*int list*tag Omd_lexer.t list) list) =
        loop2 (List.rev tmp_r) (-1) false []
      in
        (fix_lists e @ r), [], new_l
    end


  (** spaces: returns (r,p,l) where r is the result, p is the last thing read, l is the remains *)
  and spaces n r p l =
    let spaces n r previous l =
      assert (n > 0);
      match n, previous, l with (* NOT a recursive function *)
        | (1|2|3), ([]|[(Newline|Newlines _)]), (Star|Minus|Plus)::(Space|Spaces _)::tl -> (* unordered list *) 
            new_list false r [] (Newline::make_space n::l)
        | (1|2|3), ([]|[(Newline|Newlines _)]), (Number _)::Dot::(Space|Spaces _)::tl -> (* ordered list *)
            new_list true r [] (Newline::make_space n::l)
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

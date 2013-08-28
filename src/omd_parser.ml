(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(** N.B. Please do not use tabulations in your Markdown file! *)

(** flag: bold/emph using using underscores is by default
    github-style, which means that underscores inside words are left
    as underscore, rather than special characters, because it's more
    convenient. However it is also less expressive because then you
    can't bold/emph a part of a word. You might want to reverse this
    flag. *)
let gh_uemph_or_bold_style = ref true

(** flag: if true, will not check whether a used HTML tag actually
    exists in HTML. *)
let blind_html = ref false

(** flag: if true, will only accept known inline HTML tags in inline HTML. *)
let strict_html = ref false

open Printf
open Omd_backend
open Omd_lexer
open Omd_utils
open Omd_representation

(** set of known HTML codes *)
let htmlcodes_set = StringSet.of_list (* This list should be checked... *)
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

(** set of known inline HTML tags *)
let inline_htmltags_set = StringSet.of_list
  (* from https://developer.mozilla.org/en-US/docs/HTML/Inline_elements *)
  [ "b";"big";"i";"small";"tt";
    "abbr";"acronym";"cite";"code";"dfn";"em";"kbd";"strong";"samp";"var";
    "a";"bdo";"br";"img";"map";"object";"q";"script";"span";"sub";"sup";
    "button";"input";"label";"select";"textarea";]

(** N.B. it seems that there is no clear distinction between
    inline tags and block-level tags: in HTML4 it was not clear,
    in HTML5 it's even more complicated. So, the choice here is
    to specify a set of tags considered as "inline",
    cf. [inline_htmltags_set].
    So there will be inline tags, non-inline tags, and unknown tags.*)

(** All known HTML tags *)
let htmltags_set =
  (* some impossible HTML tags: body; head; html; link; meta; title *)
  StringSet.union inline_htmltags_set
    (StringSet.of_list
     [
       "a";"abbr";"acronym";"address";"applet";"area";"article";"aside"
       ;"audio";"b";"base";"basefont";"bdi";"bdo";"big";"blockquote"
       ;"br";"button";"canvas";"caption";"center";"cite";"code";"col"
       ;"colgroup";"command";"datalist";"dd";"del";"details";"dfn"
       ;"dialog";"dir";"div";"dl";"dt";"em";"embed";"fieldset"
       ;"figcaption";"figure";"font";"footer";"form";"frame";"frameset"
       ;"h1";"header";"hr";"i";"iframe";"img";"input";"ins";"kbd"
       ;"keygen";"label";"legend";"li";"map";"mark";"menu";"meter";"nav"
       ;"noframes";"noscript";"object";"ol";"optgroup";"option";"output"
       ;"p";"param";"pre";"progress";"q";"rp";"rt";"ruby";"s";"samp"
       ;"script";"section";"select";"small";"source";"span";"strike"
       ;"strong";"style";"sub";"summary";"sup";"table";"tbody";"td"
       ;"textarea";"tfoot";"th";"thead";"time";"tr";"track";"tt";"u"
       ;"ul";"var";"video";"wbr"
     ])

(** [unindent n l] returns [(unindented, rest)] where [unindented] is
    the consecutive lines of [l] that are indented with at least [n]
    spaces, and de-indented by [n] spaces. If [l] starts with a line
    that is indented by less than [n] spaces, then it returns [([], l)].
*)
let unindent n lexemes =
  let rec fix n p = function
    (* reproduce the property that twice the same token can't happen *)
    | x::tl ->
      if x = p then
        fix (n+1) p tl
      else if n = 0 then
        assert false
      else if n = 1 then
        p::fix 1 x tl
      else
        (Omd_lexer.lex(
          let b = Buffer.create n in
          for i = 1 to n do
            Buffer.add_string b(string_of_t p)
          done;
          Buffer.contents b)
        )
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

(** [is_blank l] returns [true] if [l] only contains blanks, which are
    spaces and newlines. *)
let rec is_blank = function
  | (Space | Spaces _ | Newline | Newlines _) :: tl ->
      is_blank tl
  | [] -> true
  | _ -> false

(** [fsplit_norev ?excl ~f l] returns [Some(x,y)] where [x] is the
    **reversed** list of the consecutive elements of [l] that satisfy [f].
    Note that [f] is applied to a list of elements and not just an
    element, so that [f] can look farther in the list when applied. If
    [excl] is given, then [excl] is applied before [f] is, to check if
    the splitting should be stopped right away. When the split fails,
    it returns [None]. *)
let fsplit_norev ?(excl=(fun _ -> false)) ~f l : ('a list * 'a list) option =
  let rec loop accu = function
    | [] ->
        Some(accu, [])
    | e::tl as l ->
        if excl l then
          None
        else match f l with
          | Some(left, right) ->
              Some(left@accu, right)
          | None ->
              loop (e::accu) tl
  in loop [] l

(** [fsplit ?excl ~f l] returns [Some(List.rev x, y)] if [fsplit ?excl
    ~f l] returns [Some(x,y)], else it returns [None]. *)
let fsplit ?(excl=(fun _ -> false)) ~f l =
  match fsplit_norev ~excl:excl ~f:f l with
    | None -> None
    | Some(rev, l) -> Some(List.rev rev, l)


(** [semph_or_bold n l] returns [None] if [l] doesn't start with
    a bold/emph phrase (marked using stars), else it returns [Some(x,y)]
    where [x] is the emph and/or bold phrase at the beginning of [l]
    and [y] is the rest of [l]. *)
let semph_or_bold (n:int) (l:Omd_representation.tok list) =
  (* FIXME: use rpl call/return convention *)
  assert (n>0 && n<4);
  let rec loop (result:Omd_representation.tok list) = function
    | Newline :: tl ->
      begin
        match
          fsplit_norev
            ~excl:(function Newlines _ :: _ -> true| _ -> false)
            ~f:(function (Star|Stars _ as s)::tl -> Some([s],tl) | _ -> None)
            tl
        with
        | None -> None
        | Some(_, []) -> None
        | Some((Backslash::_ as x), Star::tl) ->
          loop ((Star::x)@Newline::result) tl
        | Some(x, tl) ->
          loop (x@Newline::result) tl
      end
    | []
    | Newlines _ :: _ ->
      None
    | Backslash::Star::tl ->
      loop (Star::result) tl
    | Backslash::Stars 0::tl ->
      loop (Star::result) tl
    | Backslash::Stars n::tl ->
      loop (Star::result) (Stars(n-1)::tl)
    | (Star as t) :: tl ->
      if n = 1 then
        Some(List.rev result, tl)
      else
        loop (t :: result) tl
    | ((Stars x) as t) :: tl ->
      if n = x+2 then
        Some(List.rev result, tl)
      else
        loop (t :: result) tl
    | t::tl ->
      loop (t :: result) tl
  in
  match loop [] l with
  | None -> None
  | Some(r, tl) ->
    if is_blank r then
      None
    else
      Some(r, tl)

(** [sm_uemph_or_bold n l] returns [None] if [l] doesn't start with
    a bold/emph phrase (marked using underscores), else it returns [Some(x,y)]
    where [x] is the emph and/or bold phrase at the beginning of [l]
    and [y] is the rest of [l]. *)
let sm_uemph_or_bold (n:int) (l:Omd_representation.tok list) =
  (* FIXME: use rpl call/return convention *)
  assert (n>0 && n<4);
  let rec loop (result:Omd_representation.tok list) = function
    | Newline :: tl ->
      begin
        match
          fsplit_norev
            ~excl:(function Newlines _ :: _ -> true| _ -> false)
            ~f:(function
            | (Underscore|Underscores _ as u)::tl -> Some([u],tl)
            | _ -> None)
            tl
        with
        | None -> None
        | Some(_, []) -> None
        | Some((Backslash::_ as x), Underscore::tl) ->
          loop ((Underscore::x)@Newline::result) tl
        | Some(x, tl) ->
          loop (x@Newline::result) tl
      end
    | []
    | Newlines _ :: _ ->
      None
    | Backslash::Underscore::tl ->
      loop (Underscore::result) tl
    | Backslash::Underscores 0::tl ->
      loop (Underscore::result) tl
    | Backslash::Underscores n::tl ->
      loop (Underscore::result) (Underscores(n-1)::tl)
    | (Underscore as t) :: tl ->
      if n = 1 then
        Some(List.rev result, tl)
      else
        loop (t :: result) tl
    | ((Underscores x) as t) :: tl ->
      if n = x+2 then
        Some(List.rev result, tl)
      else
        loop (t :: result) tl
    | t::tl ->
      loop (t :: result) tl
  in
  match loop [] l with
  | None -> None
  | Some(r, tl) ->
    if is_blank r then
      None
    else
      Some(r, tl)

(** [gh_uemph_or_bold n l] returns [None] if [l] doesn't start with
    a bold/emph phrase (marked using underscores), else it returns [Some(x,y)]
    where [x] is the emph and/or bold phrase at the beginning of [l]
    and [y] is the rest of [l]. *)
let gh_uemph_or_bold (n:int) (l:Omd_representation.tok list) =
  (* FIXME: use rpl call/return convention *)
  assert (n>0 && n<4);
  let rec loop (result:Omd_representation.tok list) = function
    | Newline :: tl ->
      begin
        match
          fsplit_norev
            ~excl:(function Newlines _ :: _ -> true| _ -> false)
            ~f:(function
            |(Underscore|Underscores _ as u)::tl -> Some([u],tl)
            | _ -> None)
            tl
        with
        | None -> None
        | Some(_, []) -> None
        | Some((Backslash::_ as x), Underscore::tl) ->
          loop ((Underscore::x)@Newline::result) tl
        | Some(x, tl) ->
          loop (x@Newline::result) tl
      end
    | []
    | Newlines _ :: _ ->
      None
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
        Some(List.rev result, tl)
      else
        loop (t :: result) tl
    | ((Underscores x) as t) :: tl ->
      if n = x+2 then
        Some(List.rev result, tl)
      else
        loop (t :: result) tl
    | t::tl ->
      loop (t :: result) tl
  in
  match loop [] l with
  | None -> None
  | Some(r, tl) ->
    if is_blank r then
      None
    else
      Some(r, tl)


(** [uemph_or_bold n l] returns [None] if [l] doesn't start with a
    bold/emph phrase (marked using underscores), else it returns
    [Some(x,y)] where [x] is the emph and/or bold phrase at the
    beginning of [l] and [y] is the rest of [l]. N.B. if
    [!gh_uemph_or_bold_style] then in Github style (i.e., underscores
    inside words are considered as underscores). *)
let uemph_or_bold n l =
  (* FIXME: use rpl call/return convention *)
  if !gh_uemph_or_bold_style then
    gh_uemph_or_bold n l
  else
    sm_uemph_or_bold n l


(** [eat f l] returns [l] where elements satisfying [f] have been removed,
    but it stops removing as soon as one element doesn't satisfy [f]. *)
let rec eat f = function
  | [] -> []
  | e::tl as l -> if f e then eat f tl else l

(** [eat_blank l] returns [l] where all blanks at the beginning of the
    list have been removed (it stops removing as soon as it meets an element
    that is not a blank). Blanks are spaces and newlines only. *)
let eat_blank =
  eat (function |Space|Spaces _|Newline|Newlines _ -> true| _ -> false)

(* let split_norev f = *)
(*   let rec loop r = function *)
(*     | [] -> r, [] *)
(*     | e::tl as l -> if f e then loop (e::r) tl else r, l *)
(*   in loop [] *)

(* (\* [split f l] *\) *)
(* let split f l = *)
(*   let r, l = split_norev f l in *)
(*   List.rev r, l *)

let is_space_or_equal = function
  | Space | Spaces _ | Equal | Equals _ -> true
  |_ -> false

let is_space_or_minus = function
  | Space | Spaces _ | Minus | Minuss _ -> true
  |_ -> false

let setext_title l =
  let rec loop r = function
    | [] ->
      if r = [] then
        None
      else
        Some(List.rev r, [])
    | Newline::(Equal|Equals _|Minus|Minuss _)::tl ->
      if r = [] then
        None
      else
        Some(List.rev r, tl)
    | e::tl ->
      loop (e::r) tl
  in
  loop [] l


let tag_maybe_h1 rev_main_loop =
  Tag(fun r p l ->
    match p with
    | ([]|[Newline|Newlines _]) ->
      begin match setext_title l with
      | None ->
        None
      | Some(title, tl) ->
        let title = H1(rev_main_loop [] [] title) in
        Some((title::r), [Newline], tl)
      end
    | _ -> assert false (* -> the tag generator would be broken *)
  )

let tag_maybe_h2 rev_main_loop =
  Tag(fun r p l ->
    match p with
    | ([]|[Newline|Newlines _]) ->
      begin match setext_title l with
      | None ->
        None
      | Some(title, tl) ->
        let title = H2(rev_main_loop [] [] title) in
        Some((title::r), [Newline], tl)
      end
    | _ -> assert false (* -> the tag generator would be broken *)
  )

let tag_md md = (* [md] should be in reverse *)
  Tag(fun r p l -> Some(md@r, [], l))

(* Let's tag the lines that *might* be titles using setext-style.
   "might" because if they are, for instance, in a code section,
   then they are not titles at all. *)
let tag_setext rev_main_loop lexemes =
  let rec loop pl res = function
    | (Newline as e1)::(Equal|Equals _ as e2)::tl -> (* might be a H1. *)
      begin
        match 
          fsplit_norev
            ~f:(function
                  |(Space|Spaces _|Equal|Equals _ as e)::tl -> Some([e],tl)
                  | [] -> Some([],[])
                  |_ -> None)
            tl
        with
        | Some(rleft, (([]|(Newline|Newlines _)::_) as right)) ->
          loop [] (rleft@(e2::e1::pl@(tag_maybe_h1 rev_main_loop::res))) right
        | Some(rleft, right) ->
          loop [] (rleft@(e2::e1::pl@res)) right
        | None ->
          loop [] (e2::e1::pl@res) []
      end
    | (Newline as e1)::(Minus|Minuss _ as e2)::tl -> (* might be a H2. *)
      begin
        match
          fsplit_norev
            ~f:(function
                  |(Space|Spaces _|Minus|Minuss _ as e)::tl -> Some([e],tl)
                  | [] -> Some([],[])
                  |_ -> None)
            tl
        with
      | Some(rleft, (([]|(Newline|Newlines _)::_) as right)) ->
        loop [] (rleft@(e2::e1::pl@(tag_maybe_h2 rev_main_loop::res))) right
      | Some(rleft, right) ->
        loop [] (rleft@(e2::e1::pl@res)) right
      | None ->
        loop [] (e2::e1::pl@res) []
      end
    | (Newlines _ as e1)::tl ->
      loop [] (e1::pl@res) tl
    | e::tl ->
      loop (e::pl) res tl
    | [] ->
      pl@res
  in
  List.rev (loop [] [] lexemes)


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


(* This function is intended to "fix" lists that "look wrong" in
   Markdown. First, nothing being really "wrong" in Markdown means
   that what doesn't have a clear semantics has to be attributed
   one. Lists are based on indentation and sometimes it appears that
   we can't count on human beings to write only good looking
   Markdown. Second, it appeared hard to write a simple lists parser
   that would generate the tree that I expected to see. For those 2
   reasons, I wrote this function that fixes what's generated at some
   point by this parser. It seems to work fine.

   However, if this parser benefits from some optimizations at some
   point, one of them should probably be about getting rid of this
   function.
*)
let rec fix_lists = function
  | X _ as x :: tl ->
      x :: fix_lists tl
  | Ul[] :: tl ->
    fix_lists tl
  | Ol[] :: tl ->
    fix_lists tl
  | Ul((Ul(_) :: _ as l) :: l2) :: tl
  | Ul((Ol(_) :: _ as l) :: l2) :: tl ->
    fix_lists [Ul(l2)] @ fix_lists l @ fix_lists tl
  | Ol((Ul(_) :: _ as l) :: l2) :: tl
  | Ol((Ol(_) :: _ as l) :: l2) :: tl ->
    fix_lists [Ol(l2)] @ fix_lists l @ fix_lists tl
  | Ul(l) :: tl -> Ul(List.map (fun e -> fix_lists e) l) :: fix_lists tl
  | Ol(l) :: tl -> Ol(List.map (fun e -> fix_lists e) l) :: fix_lists tl
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
  | (Code _ |Code_block _ | Br | Hr | Ref _ | Img_ref _ | Url _ |
      Html _ | Html_block _ | Html_comments _ as e) :: tl ->
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

(* The program that generates the generated part that follows right after.
  List.iter (fun (a,b,c) ->
  print_endline ("let read_until_"^a^" ?(no_nl=false) l =
  let rec loop accu n = function
    | (Backslash as a) :: ("^b^" as b) :: tl ->
      loop (b::a::accu) n tl
    | Backslash :: ("^b^"s 0) :: tl ->
      loop ("^b^"::Backslash::accu) n ("^b^"::tl)
    | (Backslashs 0 as e) :: tl ->
      loop (e::accu) n tl
    | (Backslashs x as e) :: tl ->
      if x mod 2 = 0 then
        loop (e::accu) n tl
      else
        loop (Backslashs(x-1)::accu) n (Backslash::tl)
"^(if c<>"" then "
    | (Backslash as a) :: ("^c^" as b) :: tl ->
      loop (b::a::accu) n tl
    | Backslash :: ("^c^"s 0) :: tl ->
      loop ("^c^"::Backslash::accu) n ("^c^"::tl)
    | "^c^" as e :: tl ->
      loop (e::accu) (n+1) tl
    | "^c^"s x as e :: tl ->
      loop (e::accu) (n+x+2) tl
" else "")^
"    | "^b^" as e :: tl ->
      if n = 0 then
        List.rev accu, tl
      else
        loop (e::accu) (n-1) tl
    | "^b^"s 0 :: tl ->
      if n = 0 then
        List.rev accu, "^b^"::tl
      else
        loop ("^b^"::accu) (n-1) ("^b^"::tl)
    | "^b^"s n :: tl ->
      List.rev accu, "^b^"s(n-1)::tl
    | (Newline|Newlines _ as e)::tl ->
      if no_nl then
        raise NL_exception
      else
        loop (e::accu) n tl
    | e::tl ->
      loop (e::accu) n tl
    | [] ->
      raise Premature_ending
  in loop [] 0 l
"))

[ "gt", "Greaterthan", "Lessthan";
  "lt", "Lessthan", "";
  "cparenth", "Cparenthesis", "Oparenthesis";
  "oparenth", "Oparenthesis", "";
  "dq", "Doublequote", "";
  "q", "Quote", "";
  "obracket", "Obracket", "";
  "cbracket", "Cbracket", "Obracket";
  "space", "Space", "";
  "newline", "Newline", "";
  ]
*)

(* begin generated part *)
let read_until_gt ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Greaterthan as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Greaterthans 0 :: tl ->
        loop (Greaterthan :: Backslash :: accu) n (Greaterthan :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Backslash as a)) :: ((Lessthan as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Lessthans 0 :: tl ->
        loop (Lessthan :: Backslash :: accu) n (Lessthan :: tl)
    | ((Lessthan as e)) :: tl -> loop (e :: accu) (n + 1) tl
    | ((Lessthans x as e)) :: tl -> loop (e :: accu) ((n + x) + 2) tl
    | ((Greaterthan as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Greaterthans 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Greaterthan :: tl))
        else loop (Greaterthan :: accu) (n - 1) (Greaterthan :: tl)
    | Greaterthans n :: tl ->
        ((List.rev accu), ((Greaterthans (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_lt ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Lessthan as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Lessthans 0 :: tl ->
        loop (Lessthan :: Backslash :: accu) n (Lessthan :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Lessthan as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Lessthans 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Lessthan :: tl))
        else loop (Lessthan :: accu) (n - 1) (Lessthan :: tl)
    | Lessthans n :: tl -> ((List.rev accu), ((Lessthans (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_cparenth ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Cparenthesis as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Cparenthesiss 0 :: tl ->
        loop (Cparenthesis :: Backslash :: accu) n (Cparenthesis :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Backslash as a)) :: ((Oparenthesis as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Oparenthesiss 0 :: tl ->
        loop (Oparenthesis :: Backslash :: accu) n (Oparenthesis :: tl)
    | ((Oparenthesis as e)) :: tl -> loop (e :: accu) (n + 1) tl
    | ((Oparenthesiss x as e)) :: tl -> loop (e :: accu) ((n + x) + 2) tl
    | ((Cparenthesis as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Cparenthesiss 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Cparenthesis :: tl))
        else loop (Cparenthesis :: accu) (n - 1) (Cparenthesis :: tl)
    | Cparenthesiss n :: tl ->
        ((List.rev accu), ((Cparenthesiss (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_oparenth ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Oparenthesis as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Oparenthesiss 0 :: tl ->
        loop (Oparenthesis :: Backslash :: accu) n (Oparenthesis :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Oparenthesis as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Oparenthesiss 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Oparenthesis :: tl))
        else loop (Oparenthesis :: accu) (n - 1) (Oparenthesis :: tl)
    | Oparenthesiss n :: tl ->
        ((List.rev accu), ((Oparenthesiss (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_dq ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Doublequote as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Doublequotes 0 :: tl ->
        loop (Doublequote :: Backslash :: accu) n (Doublequote :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Doublequote as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Doublequotes 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Doublequote :: tl))
        else loop (Doublequote :: accu) (n - 1) (Doublequote :: tl)
    | Doublequotes n :: tl ->
        ((List.rev accu), ((Doublequotes (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_q ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Quote as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Quotes 0 :: tl ->
        loop (Quote :: Backslash :: accu) n (Quote :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Quote as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Quotes 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Quote :: tl))
        else loop (Quote :: accu) (n - 1) (Quote :: tl)
    | Quotes n :: tl -> ((List.rev accu), ((Quotes (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_obracket ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Obracket as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Obrackets 0 :: tl ->
        loop (Obracket :: Backslash :: accu) n (Obracket :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Obracket as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Obrackets 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Obracket :: tl))
        else loop (Obracket :: accu) (n - 1) (Obracket :: tl)
    | Obrackets n :: tl -> ((List.rev accu), ((Obrackets (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_cbracket ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Cbracket as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Cbrackets 0 :: tl ->
        loop (Cbracket :: Backslash :: accu) n (Cbracket :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Backslash as a)) :: ((Obracket as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Obrackets 0 :: tl ->
        loop (Obracket :: Backslash :: accu) n (Obracket :: tl)
    | ((Obracket as e)) :: tl -> loop (e :: accu) (n + 1) tl
    | ((Obrackets x as e)) :: tl -> loop (e :: accu) ((n + x) + 2) tl
    | ((Cbracket as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Cbrackets 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Cbracket :: tl))
        else loop (Cbracket :: accu) (n - 1) (Cbracket :: tl)
    | Cbrackets n :: tl -> ((List.rev accu), ((Cbrackets (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_space ?(no_nl = false) l =
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Space as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Spaces 0 :: tl ->
        loop (Space :: Backslash :: accu) n (Space :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Space as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Spaces 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Space :: tl))
        else loop (Space :: accu) (n - 1) (Space :: tl)
    | Spaces n :: tl -> ((List.rev accu), ((Spaces (n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_newline l = (* this has been patched post-generation *)
  let rec loop accu n =
    function
    | ((Backslash as a)) :: ((Newline as b)) :: tl ->
        loop (b :: a :: accu) n tl
    | Backslash :: Newlines 0 :: tl ->
        loop (Newline :: Backslash :: accu) n (Newline :: tl)
    | ((Backslashs 0 as e)) :: tl -> loop (e :: accu) n tl
    | ((Backslashs x as e)) :: tl ->
        if (x mod 2) = 0
        then loop (e :: accu) n tl
        else loop ((Backslashs (x - 1)) :: accu) n (Backslash :: tl)
    | ((Newline as e)) :: tl ->
        if n = 0 then ((List.rev accu), tl) else loop (e :: accu) (n - 1) tl
    | Newlines 0 :: tl ->
        if n = 0
        then ((List.rev accu), (Newline :: tl))
        else loop (Newline :: accu) (n - 1) (Newline :: tl)
    | Newlines n :: tl -> ((List.rev accu), ((Newlines (n - 1)) :: tl))
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l
(* /end generated part *)




(* H1, H2, H3, ... *)
let read_title rev_main_loop n r p l =
  let title, rest =
    let rec loop accu = function
      | ((Hash|Hashs _)::((Newline|Newlines _)::_ as l))
      | ((Hash|Hashs _)::(Space|Spaces _)::
           ((Newline|Newlines _)::_ as l))
      | ((Newline|Newlines _)::_ as l)
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



(** [maybe_extension e r p l] returns None if there is no extension or
    if extensions haven't had  any effect, returns Some(nr, np, nl) if
    at least one extension has applied successfully. *)
let maybe_extension extensions r p l =
  match extensions with
  | [] -> None
  | _ ->
    List.fold_left
      (function
      | None ->
        (fun f -> f r p l)
      | Some(nr, np, nl) as e ->
        (fun f -> match f nr np nl with None -> e | Some _ as k -> k)
      )
      None
      extensions

let emailstyle_quoting rev_main_loop r _p lexemes =
  let rec loop block cl =
    function
    | Newline::Greaterthan::(Newline::_ as tl) ->
      loop (Newline::cl@block) [] tl
    | Newline::Greaterthan::Space::tl ->
      loop (Newline::cl@block) [] tl
    | Newline::Greaterthan::Spaces 0::tl ->
      loop (Newline::cl@block) [Space] tl
    | Newline::Greaterthan::Spaces n::tl ->
      loop (Newline::cl@block) [Spaces(n-1)] tl
      (* | Newline::tl -> loop block (Newline::cl) tl *)
    | (Newlines _::_ as l) | ([] as l) -> List.rev (cl@block), l
    | e::tl -> loop block (e::cl) tl
  in
  match loop [] [] lexemes with
  | block, tl ->
    if debug then
      eprintf "##############################\n%s\n\
                  ##############################\n%!" (string_of_tl block);
    (Blockquote(rev_main_loop [] [] block)::r), [Newline], tl

(* maybe a reference *)
let maybe_reference ___rev_main_loop rc r p l =
  (* this function is called when we know it's not a link although
     it started with a '[' *)
  (* So it could be a reference or a link definition. *)
  let rec maybe_ref l =
    let text, remains = read_until_cbracket l in
    if (try ignore(read_until_obracket text); true
        with Premature_ending -> false) then
      raise Premature_ending;
    let blank, remains = read_until_obracket remains in
    if eat (function | (Space|Spaces _|Newline|Newlines _) -> true
                     | _ -> false) blank <> [] then raise Premature_ending;
    match read_until_cbracket remains with
    | [], remains ->
      let id = string_of_tl text in (* implicit anchor *)
      Some(((Ref(rc, id, id))::r), [Cbracket], remains)
    | id, remains ->
      Some(((Ref(rc, string_of_tl id, string_of_tl text))::r),
           [Cbracket], remains)
  in
  let rec maybe_def l =
    match read_until_cbracket l with
    | _, [] -> None
    | id, (Colon::(Space|Spaces _)::remains)
    | id, (Colon::remains) ->
        begin
          match
            fsplit
              ~f:(function
                    | (Space|Spaces _|Newline|Newlines _)::_ -> None
                    | e::tl -> Some([e],tl)
                    | [] -> None)
              remains
          with
            | None -> raise Premature_ending
            | Some(url, remains) ->
                let title, remains =
                  match
                    eat
                      (function | (Space|Spaces _|Newline|Newlines _) -> true
                         | _ -> false)
                      remains
                  with
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
    try
      maybe_ref l
    with | Premature_ending | NL_exception ->
      try
        maybe_def l 
      with
      | Premature_ending | NL_exception -> None

(* maybe a link *)
let maybe_link rev_main_loop r p l =
  let read_title name url l =
    match l with
    | Doublequote::l ->
      begin
        match read_until_dq l with
        | title, (Cparenthesis::tl)
        | title, ((Space|Spaces _)::Cparenthesis::tl) ->
          Some(Url(url,name,string_of_tl title)::r,[Cparenthesis],tl)
        | title, (Cparenthesiss 0::tl)
        | title, ((Space|Spaces _)::Cparenthesiss 0::tl) ->
          Some(Url(url,name,string_of_tl title)::r,
               [Cparenthesis],
               Cparenthesis::tl)
        | title, (Cparenthesiss n::tl)
        | title, ((Space|Spaces _)::Cparenthesiss n::tl) ->
          Some(Url(url,name,string_of_tl title)::r,
               [Cparenthesis],
               Cparenthesiss(n-1)::tl)
        | x,y ->
          eprintf "x=%s y=%s\n%!"
            (destring_of_tl x) (destring_of_tl y);
          None
      end
    | _ -> None
  in
  let read_url name l =
    try
      try
        let url, tl = read_until_space ~no_nl:true l in
        try
          ignore(read_until_cparenth url);
          (* there actually is no title *)
          let url, tl = read_until_cparenth ~no_nl:true l in
            Some(Url(string_of_tl url,name,"")::r, [Cparenthesis],tl)
        with Premature_ending ->
          read_title name (string_of_tl url) (eat_blank tl)
      with
      | NL_exception ->
        let url, tl = read_until_newline l in
        try
          ignore(read_until_cparenth url);
          (* there actually is no title *)
          let url, tl = read_until_cparenth ~no_nl:true l in
            Some(Url(string_of_tl url,name,"")::r, [Cparenthesis],tl)
        with Premature_ending ->
        read_title name (string_of_tl url) (eat_blank tl)
    with
    | Premature_ending ->
      let url, tl = read_until_cparenth l in
      Some(Url(string_of_tl url, name, "")::r,[Cparenthesis],tl)
  in
  let read_name l =
    try
      match read_until_cbracket l with
      | name, (Oparenthesis::tl) ->
        read_url (rev_main_loop [] [] name) (eat_blank tl)
      | _ -> 
        None
    with Premature_ending | NL_exception -> None
  in
  read_name l

(** code that starts with one or several backquote(s) *)
let bcode r p l =
  let e, tl =
    match l with
    | (Backquote|Backquotes _ as e)::tl -> e, tl
    | _ -> (* bcode is wrongly called *) assert false 
  in
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
  if List.exists (function (Newline|Newlines _) -> true | _ -> false) cb
  then
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

let icode r p l =
  (* indented code:
     returns (r,p,l) where r is the result, p is the last thing read,
     l is the remains *)
  let dummy_tag = Tag(fun r p l -> None) in
  let accu = Buffer.create 42 in
  let rec loop = function
    | (Newline|Newlines _ as p), ((Space|Spaces(0|1))::_ as tl) ->
      (* 1, 2 or 3 spaces. *)
      (* -> Return what's been found as code because what follows isn't. *)
      Code_block (Buffer.contents accu)::r, [p], tl
    | (Newline|Newlines _ as p), Spaces(n)::tl ->
      (* At least 4 spaces, it's still code. *)
      Buffer.add_string accu (string_of_t p);
      loop ((if n >= 4 then Spaces(n-4) else if n = 3 then Space else dummy_tag), tl)
    | (Newline|Newlines _ as p), (not_spaces::_ as tl) -> (* stop *)
      Code_block (Buffer.contents accu)::r, [p], tl
        (* -> Return what's been found as code because it's no more code. *)
    | p, e::tl ->
      Buffer.add_string accu (string_of_t p);
      (* html entities are to be converted later! *)
      loop (e, tl)
    | p, [] ->
      Buffer.add_string accu (string_of_t p);
      Code_block (Buffer.contents accu)::r, [p], []
  in
    match l with
      | Spaces n::tl ->
          loop ((if n >= 4 then Spaces(n-4) else if n = 3 then Space else dummy_tag), tl)
      | _ -> assert false

(** new_list: returns (r,p,l) where r is the result, p is the last thing
    read, l is the remains *)
    (* FIXME: make [o] use [type o = Ordered | Unordered] instead of [bool]  *)
let new_list rev_main_loop main_loop (o:bool) r p l =
  if debug then
    eprintf "new_list p=(%s) l=(%s)\n%!" (destring_of_tl p)
      (destring_of_tl l);
  let list_hd e = match e with hd::_ -> hd | _ -> 0 in
  let rec loop (fi:bool) (ordered:bool)
      (result:(bool*int list*Omd_representation.tok list)list)
      (curr_item:Omd_representation.tok list)
      (indents:int list)
      (lexemes:Omd_representation.tok list) =
        (* 'fi' means first iteration *)
    let er = if debug then
        let to_string r (o,il,e) =
          r ^ sprintf "(%b," o ^ destring_of_tl e ^ ")" in
        List.fold_left to_string "" result
      else "" in
    if debug then
      eprintf "new_list>>loop er=(%s) curr_item=(%s) lexemes=%s\n%!"
        er (destring_of_tl curr_item) (destring_of_tl lexemes);
    match lexemes with
        (* Boolean is true if ordered, false otherwise. *)
        (* first loop: return the list of (indentation level * item) *)
        (* indent = 0 *)
    | (Newline|Newlines 0) :: (Star|Minus|Plus) :: (Space|Spaces _) :: tl ->
      if debug then eprintf "#%d\n%!" 1;
      if fi then
        loop false ordered result [] (0::indents) tl
      else
        loop false false ((false,indents,curr_item)::result) []
          (0::indents) tl
    | (Newline|Newlines 0) :: (Number _) :: Dot :: (Space|Spaces _) :: tl ->
      if debug then Printf.eprintf "#%d\n%!" 2;
      if fi then
        loop false ordered result [] (0::indents) tl
      else
        loop false true ((true,indents,curr_item)::result) []
          (0::indents) tl

        (* indent = 1 *)
    | (Newline|Newlines 0) :: Space :: (Star|Minus|Plus)
      :: (Space|Spaces _) :: tl ->
      if debug then Printf.eprintf "#%d\n%!" 3;
        if fi then
          loop false ordered result [] (1::indents) tl
        else
          loop false false ((false,indents,curr_item)::result) []
            (1::indents) tl
    | (Newline|Newlines 0) :: Space :: Number _ :: Dot
      :: (Space|Spaces _) :: tl ->
      if debug then Printf.eprintf "#%d\n%!" 4;
        if fi then
          loop false ordered result [] (1::indents) tl
        else
          loop false true ((true,indents,curr_item)::result) []
            (1::indents) tl

        (* indent >= 2 *)
    | (Newline|Newlines 0) :: ((Spaces(x) :: (Star|Minus|Plus)
                                :: (Space|Spaces _) :: tl) as p) ->
      if debug then Printf.eprintf "#%d\n%!" 5;
      if x+2 > list_hd indents + 4 then
            (* a single new line & too many spaces -> *not* a new list item. *)
        loop false ordered result curr_item indents p
          (* p is what follows the new line *)
      else
            (* a new list item, set previous current item as a complete item *)
        if fi then
          loop false ordered result [] ((x+2)::indents) tl
        else
          loop false false ((false,indents,curr_item)::result)
            [] ((x+2)::indents) tl

    | (Newline|Newlines 0) :: ((Spaces(x) :: Number _ :: Dot
                                :: (Space|Spaces _) :: tl) as p) ->
      if debug then Printf.eprintf "#%d\n%!" 6;
      if x+2 > list_hd indents + 4 then
        (* a single new line & too many spaces -> *not* a new list item. *)
        loop false ordered result curr_item indents p
      (* p is what follows the new line *)
      else
        (* a new list item, set previous current item as a complete item *)
        if fi then
          loop false ordered result [] ((x+2)::indents) tl
        else
          loop false true ((true,indents,curr_item)::result) []
            ((x+2)::indents) tl
    | Newlines(0) :: ((Spaces(2|3|4|5 as n)) :: Greaterthan
                      :: (Space|Spaces _) :: tl as l) ->
          (* blockquote inside a list *)
      let block, rest = unindent (n+2) (Newline::l) in
      let em, _, _x = emailstyle_quoting rev_main_loop [] [] block in
      assert(_x = []);
      loop false ordered result (tag_md(em)::curr_item) indents rest
    | Newlines(0) :: (Spaces(n) :: tl as l)
    | Newline::Newline:: (Spaces(n) :: tl as l)
        when (try n+2 >= List.hd indents+4 with _ -> assert false) ->
          (* code inside a list *)
      let block, rest = unindent (List.hd indents+4) (Newline::l) in
      loop false ordered result
        (tag_md(main_loop [] [] block)::curr_item) indents rest

    | ((Newline|Newlines 0 as k) :: Spaces(_) :: e :: tl) ->
          (* adding e to the current item *)
      if debug then eprintf "#%d (%s)\n%!" 88 (destring_of_tl lexemes);
      loop false ordered result (e::Space::k::curr_item) indents tl

    | (Newline as k) :: e :: tl ->
          (* adding e to the current item *)
      if debug then eprintf "#%d (%s)\n%!" 8 (destring_of_tl lexemes);
      loop false ordered result (e::k::curr_item) indents tl

    | Newlines 0 :: (Tag _|Hash|Hashs _) :: _ ->
      (* FIXME: do something else when Tag *)
      (* Tricky: 2 line breaks, but we're suspecting a H1..H6 and
         it's probably going to be the case, hence we're out of
         the list. *)
      ((ordered,indents,curr_item)::result, lexemes)

    | (Newlines 0 as k) :: e :: tl ->
          (* adding e to the current item *)
      if debug then eprintf "#%d (%s)\n%!" 8 (destring_of_tl lexemes);
      loop false ordered result (e::k::curr_item) indents tl

    | ([] | (Newlines(_) :: _)) ->
      if debug then eprintf "#%d******************************\n%!" 7;
          (* if an empty line appears, then it's the end of the list(s). *)
      ((ordered,indents,curr_item)::result, lexemes)

    | e :: tl -> (* adding e to the current item *)
      if debug then eprintf "#%d (%s)\n%!" 9 (destring_of_tl lexemes);
      loop false ordered result (e::curr_item) indents tl
  in
  let rec loop2 (tmp:(bool*int list*Omd_representation.tok list) list)
      (curr_indent:int) (ordered:bool) (accu:t list)
      : t * (bool*int list*Omd_representation.tok list) list =
    let er = if debug then
        let to_string r (o,il,e) =
          r ^ sprintf "(%b," o ^ destring_of_tl e ^ ")" in
        List.fold_left to_string "" tmp
      else "" in
    if debug then Printf.eprintf "new_list>>loop2\n%!";
    match tmp with
    | (o,(i::indents), item) :: tl ->
      if debug then Printf.eprintf "@338:loop2 tmp=%s\n%!" er;
      let item = List.rev item in
      if i = curr_indent then (
        if debug then Printf.eprintf "PLOP\n%!";
        loop2 tl i ordered ((rev_main_loop [] [Space;Star] item)::accu)
      )
      else if i > curr_indent then ( (* new sub list *)
        if debug then Printf.eprintf "NEW SUB LIST\n%!";
        let md, new_tl =
          loop2 tl i o [rev_main_loop [] [Space;Star] item] in
        match accu with
        | hd :: accu_tl ->
          loop2 new_tl curr_indent ordered ((hd@md) :: accu_tl)
        | [] ->
          if curr_indent = -1 then
            md, new_tl
          else
            loop2 new_tl curr_indent ordered [md]
      )
      else (* i < curr_indent *)
        let accu = List.rev accu in
        [if ordered then Ol accu else Ul accu], tmp
    | [(_,[],[])]
    | [] ->
      if debug then eprintf "FOO\n%!";
      if accu = [] then [], []
      else
        let accu = List.rev accu in
        [if ordered then Ol accu else Ul accu], []
    | (o,[], item) :: tl ->
      if debug then
        eprintf "@386:loop2 tmp=(%b,[],%s)::(%n)\n%!" o
          ((destring_of_tl item)) (List.length tl);
      loop2 ((o,[0], item) :: tl) curr_indent ordered accu
  in
  let tmp_r, new_l = loop true o [] [] [] l in
      (* tmp_r: (bool*int list*Omd_representation.tok list) list) ;
         new_l:Omd_representation.tok list *)
  if debug then (
    let p =
      List.fold_left
        (fun r (o,indents,item) ->
          sprintf "%s(%b,#%d,%s)::" r o (List.length indents)
            (destring_of_tl item))
        ""
        (List.rev tmp_r) in
    eprintf "tmp_r=%s[] new_l=%s\n%!" (p) ("")
  );
  let (e:t), (x:(bool*int list*Omd_representation.tok list) list) =
    loop2 (List.rev tmp_r) (-1) false []
  in
  (fix_lists e @ r), [], new_l

(** spaces: returns (r,p,l) where r is the result, p is the last thing
      read, l is the remains *)
let spaces rev_main_loop main_loop n r p l =
  let spaces n r previous l =
    assert (n > 0);
    match n, previous, l with (* NOT a recursive function *)
    | (1|2|3), ([]|[(Newline|Newlines _)]), (Star|Minus|Plus)
      ::(Space|Spaces _)::tl ->
          (* unordered list *)
      new_list rev_main_loop main_loop false r [] (Newline::make_space n::l)
    | (1|2|3), ([]|[(Newline|Newlines _)]), (Number _)::Dot
      ::(Space|Spaces _)::tl ->
          (* ordered list *)
      new_list rev_main_loop main_loop true r [] (Newline::make_space n::l)
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




let main_parse extensions lexemes =
  let rc = new Omd_representation.ref_container in

  (* [main_loop ] should be called only by itself and [rev_main_loop ] *)
  let rec main_loop (r:t) (previous:Omd_representation.tok list)
      (lexemes:Omd_representation.tok list) =
    if debug then
      eprintf "main_loop r=%s p=(%s) l=(%s)\n%!"
        (Omd_backend.sexpr_of_md (List.rev r))
        (destring_of_tl previous) (destring_of_tl lexemes);
    match previous, lexemes with
    (* no more to process *)
    | _, [] ->
      (* return the result (/!\ it has to be reversed as some point) *)
      r

      (* Tag: tag system $\cup$ high-priority extension mechanism *)
    | _, Tag(e) :: tl ->
      begin match e r previous tl with
      | Some(r, p, l) ->
        main_loop r p l
      | None ->
        main_loop r previous tl
      end

    (* HTML comments *)
    | _, (Lessthan as t)::(Exclamation::Minuss 0::c as tl) ->
      begin
        let f = function
          | (Minuss _ as m)::(Greaterthan|Greaterthans _ as g)::tl ->
            Some([g;m], tl)
          | _ -> None
        in
        match fsplit ~f:f lexemes with
        | None ->
          begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop (Text(string_of_t t)::r) [t] tl
          | Some(r, p, l) -> main_loop r p l
          end
        | Some (comments, new_tl) ->
          main_loop
            (Html_comments(string_of_tl comments)::r)
            [Greaterthan]
            new_tl

      end

    (* email-style quoting *)
    | ([]|[Newline|Newlines _]), Greaterthan::(Space|Spaces _)::_ ->
      begin
        let r, p, l =
          emailstyle_quoting rev_main_loop r previous (Newline::lexemes)
        in main_loop r p l
      end

    (* email-style quoting, with lines starting with spaces! *)
    | ([]|[Newline|Newlines _]), (Space|Spaces(0|1) as s)
      :: Greaterthan :: (Space|Spaces _)::_ ->
      (* It's 1, 2 or 3 spaces, not more because it wouldn't mean
         quoting anymore but code. *)
      begin
        let new_r, p, rest =
          let foo, rest = unindent (fst(length s)) (Newline::lexemes) in
          match
            emailstyle_quoting rev_main_loop [] previous (Newline::(foo))
          with
          | new_r, p, [] -> new_r, p, rest
          | _ -> assert false
        in
        main_loop (new_r@r) [Newline] rest
      end

    (* minus *)
    | ([]|[Newline|Newlines _]), (Minus|Minuss _)::(Space|Spaces _) ::_ ->
      (* maybe hr *)
      begin match hr_m lexemes with
      | None -> (* no hr, so it's a list *)
        let md, new_p, new_l =
          new_list rev_main_loop main_loop false r [] (Newline::lexemes)
        in
        main_loop md new_p new_l
      | Some l -> (* hr *)
        main_loop (Hr::r) [Newline] l
      end
    | ([]|[Newline|Newlines _]), (Minus|Minuss _ as t)::tl ->
      begin match hr_m lexemes with
      | None -> (* no hr, but it's not a list *)
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      | Some l -> (* hr *)
        main_loop (Hr::r) [Newline] l
      end

    (* hashes *)
    | ([]|[(Newline|Newlines _)]), Hashs n :: tl -> (* hash titles *)
      let r, p, l = read_title rev_main_loop (n+2) r previous tl in
      main_loop r p l
    | ([]|[(Newline|Newlines _)]), Hash :: tl -> (* hash titles *)
      let r, p, l = read_title rev_main_loop 1 r previous tl in
      main_loop r p l
    | _, (Hash|Hashs _ as t) :: tl -> (* hash -- no title *)
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t t)::r) [t] tl
      | Some(r, p, l) -> main_loop r p l
      end

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
          let r, p, l =
            spaces rev_main_loop main_loop (fst (length t)) r previous tl
          in
          main_loop r p l
        | Some l ->
          main_loop (Hr::r) [Newline] l
        end
      | Some l ->
        main_loop (Hr::r) [Newline] l
      end

    (* spaces anywhere *)
    | _, ((Space|Spaces _) as t) :: tl ->
      (* too many cases to be handled here *)
      let r, p, l =
        spaces rev_main_loop main_loop (fst (length t)) r previous tl
      in
      main_loop r p l

    (* underscores *)
    | _, (Underscore as t) :: tl -> (* one "orphan" underscore, or emph *)
      (match uemph_or_bold 1 tl with
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      | Some(x, new_tl) ->
        main_loop (Emph(rev_main_loop [] [t] x) :: r) [t] new_tl
      )
    | _, (Underscores((0|1) as n) as t) :: tl ->
      (* 2 or 3 "orphan" underscores, or emph/bold *)
      (match uemph_or_bold (n+2) tl with
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      | Some(x, new_tl) ->
        if n = 0 then (* 1 underscore *)
          main_loop (Bold(rev_main_loop [] [t] x) :: r) [t] new_tl
        else (* 2 underscores *)
          main_loop (Emph([Bold(rev_main_loop [] [t] x)]) :: r) [t] new_tl
      )

    (* enumerated lists *)
    | ([]|[Newline|Newlines _]), (Number _) :: Dot :: (Space|Spaces _) :: tl ->
      let md, new_p, new_l =
        new_list rev_main_loop main_loop true r [] (Newline::lexemes)
      in
      main_loop md new_p new_l

    (* stars *)
    | ([]|[(Newline|Newlines _)]), Star :: (Space|Spaces _) :: _ ->
      (* maybe hr or new list *)
      begin match hr_s lexemes with
      | Some l ->
        main_loop (Hr::r) [Newline] l
      | None ->
        let md, new_p, new_l =
          new_list rev_main_loop main_loop false r [] (Newline::lexemes)
        in
        main_loop md new_p new_l
      end
    | ([]|[(Newline|Newlines _)]), Stars _ :: _ when hr_s lexemes <> None ->
      (* hr *)
      (match hr_s lexemes with
      | Some l -> main_loop (Hr::r) [Newline] l
      | None -> assert false
      )
    | ([]|[(Newline|Newlines _)]), (Star as t) :: tl -> (* maybe hr *)
      begin match hr_s lexemes with
      | Some l ->
        main_loop (Hr::r) [Newline] l
      | None ->
        (match semph_or_bold 1 tl with
        | Some(x, new_tl) ->
          main_loop (Emph(rev_main_loop [] [t] x) :: r) [t] new_tl
        | None ->
          begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop (Text(string_of_t t)::r) [t] tl
          | Some(r, p, l) -> main_loop r p l
          end
        )
      end
    | _, (Star as t) :: tl -> (* one "orphan" star, or emph // can't be hr *)
      (match semph_or_bold 1 tl with
      | Some(x, new_tl) ->
        main_loop (Emph(rev_main_loop [] [t] x) :: r) [t] new_tl
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      )
    | _, (Stars((0|1) as n) as t) :: tl ->
      (* 2 or 3 "orphan" stars, or emph/bold *)
      (match semph_or_bold (n+2) tl with
      | Some(x, new_tl) ->
        if n = 0 then
          main_loop (Bold(rev_main_loop [] [t] x) :: r) [t] new_tl
        else
          main_loop (Emph([Bold(rev_main_loop [] [t] x)]) :: r) [t] new_tl
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      )

    (* backslashes *)
    | _, Backslash :: (Newline as t) :: tl -> (* \\n *)
      main_loop (Br :: r) [t] tl
    | _, Backslash :: Newlines 0 :: tl -> (* \\n\n\n\n... *)
      main_loop (Br :: r) [Backslash; Newline] (Newline :: tl)
    | _, Backslash :: Newlines n :: tl -> assert (n >= 0); (* \\n\n\n\n... *)
      main_loop (Br :: r) [Backslash; Newline]
        (Newlines (n-1) :: tl)
    | _, Backslash :: (Backquote as t) :: tl -> (* \` *)
      main_loop (Text ("`") :: r) [t] tl
    | _, Backslash :: Backquotes 0 :: tl -> (* \````... *)
      main_loop (Text ("`") :: r) [Backslash; Backquote] (Backquote :: tl)
    | _, Backslash :: Backquotes n :: tl -> assert (n >= 0); (* \````... *)
      main_loop (Text ("`") :: r) [Backslash; Backquote]
        (Backquotes (n-1) :: tl)
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
      main_loop (Text ("_") :: r) [Backslash; Underscore]
        (Underscores (n-1) :: tl)
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
      main_loop (Text ("(") :: r) [Backslash; Oparenthesis]
        (Oparenthesiss (n-1) :: tl)
    | _, Backslash :: (Cparenthesis as t) :: tl -> (* \) *)
      main_loop (Text (")") :: r) [t] tl
    | _, Backslash :: Cparenthesiss 0 :: tl -> (* \)))... *)
      main_loop (Text (")") :: r) [Backslash; Cparenthesis]
        (Cparenthesis :: tl)
    | _, Backslash :: Cparenthesiss n :: tl -> assert (n >= 0); (* \)))... *)
      main_loop (Text (")") :: r) [Backslash; Cparenthesis]
        (Cparenthesiss (n-1) :: tl)
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
      main_loop (Text ("!") :: r) [Backslash; Exclamation]
        (Exclamations (n-1) :: tl)
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
    | _, (Lessthan|Lessthans _ as t)
      :: (Word("http"|"https"|"ftp"|"ftps"|"ssh"|"afp"|"imap") as w)
      :: Colon::Slashs(n)::tl ->
      (* "semi-automatic" URLs *)
      let rec read_url accu = function
        | (Newline|Newlines _)::tl ->
          None
        | Greaterthan::tl ->
          let url =
            (match t with Lessthans 0 -> "<"
            | Lessthans n -> String.make (n-2) '<' | _ -> "")
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
        main_loop (Url(url,[Text url],"")::r) [] new_tl
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      end

    (* Word(w) *)
    | _, Word w::tl ->
      main_loop (Text w :: r) [Word w] tl

    (* newline at the end *)
    | _, [Newline] ->
      NL::r

    (* named html entity *)
    | _, Ampersand::((Word w::((Semicolon|Semicolons _) as s)::tl) as tl2) ->
      if StringSet.mem w htmlcodes_set then
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
    | _, Ampersand::((Hash::Number w::((Semicolon|Semicolons _) as s)::tl)
                        as tl2) ->
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
    (* block html *)
    | ([]|[Newline|Newlines _]), (Lessthan|Lessthans _ as t)::
      Word(tagname)
      ::((Space|Spaces _|Greaterthan|Greaterthans _) as x)
      ::tl ->
      if StringSet.mem tagname inline_htmltags_set then
        main_loop r [Word ""] lexemes
      else if not (!blind_html || StringSet.mem tagname htmltags_set) then
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      else
        let read_html() =
          let rec loop accu n = function
            | Lessthan::Slash::Word(tn)::Greaterthan::tl ->
              if tn = tagname then
                if n = 0 then
                  List.rev (Greaterthan::Word(tn)::Slash::Lessthan::accu), tl
                else
                  loop (Greaterthan::Word(tn)::Slash::Lessthan::accu)
                    (n-1) tl
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
        let r = match t with
          | Lessthan -> r
          | Lessthans 0 -> Text("<")::r
          | Lessthans n -> Text(String.make (n-3) '<')::r
          | _ -> assert false
        in
        let html, tl = read_html() in
        main_loop (Html_block(string_of_tl html)::r) [Greaterthan] tl

    (* inline html *)
    | _, (Lessthan|Lessthans _ as t)::
      (Word(tagname) as w)
      ::((Space|Spaces _|Greaterthan|Greaterthans _ as x)
         ::tl as l) ->
      if (!strict_html && not(StringSet.mem tagname inline_htmltags_set))
        || not(!blind_html || StringSet.mem tagname htmltags_set)
      then
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop r p l
        end
      else
        let read_html() =
          let tag s = tag_md [Html s] in
          let rec loop accu n = function
            | Lessthan::Word("img"|"br"|"hr" as tn)::tl ->
              (* self-closing tags *)
              if n = 0 then
                let b, tl = read_until_gt tl in
                ((tag(sprintf "<%s%s>" tn (string_of_tl b))) ::accu), tl
              else
                let b, tl = read_until_gt tl in
                loop (tag(sprintf "<%s%s>" tn (string_of_tl b))::accu) n tl
            | Lessthan::Slash::Word(tn)::Greaterthan::tl -> (* </word> ... *)
              if tn = tagname then
                if n = 0 then
                  List.rev (tag(sprintf "</%s>" tn)::accu), tl
                else
                  loop (tag(sprintf "</%s>" tn)::accu) (n-1) tl
              else
                loop (Word(sprintf "</%s>" tn)::accu) n tl
            | Lessthan::Word(tn)::tl -> (* <word... *)
              let b, tl = read_until_gt tl in
              if tn = tagname then
                loop (tag(sprintf "<%s%s>" tn (string_of_tl b))::accu)
                  (n+1) tl
              else
                loop (tag(Printf.sprintf "<%s%s>" tn (string_of_tl b))
                      :: accu) n tl
            | x::tl ->
              loop (x::accu) n tl
            | [] ->
              List.rev accu, []
          in
          let b, tl = read_until_gt l in
          if (try ignore(read_until_lt b); false
            with Premature_ending -> true) then
            (* there must not be any '<' in b *)
            loop [tag(Printf.sprintf "<%s%s%s>" tagname (string_of_t x)
                        (string_of_tl b))] 0 tl
          else
            raise Premature_ending
        in
        (match try Some(read_html()) with Premature_ending -> None with
        | Some(html, tl) ->
          let r = match t with
            | Lessthan -> r
            | Lessthans 0 -> Text("<")::r
            | Lessthans n -> Text(String.make (n-3) '<')::r
            | _ -> assert false
          in
          main_loop (main_loop [] [] html @ r) [Greaterthan] tl
        | None ->
          main_loop (Text(string_of_t t^tagname)::r) [w] l
        )
    (* / end of inline HTML. *)


    (* line breaks *)
    | _, Newline::tl ->
      main_loop (NL::r) [Newline] tl
    | _, Newlines _::tl ->
      main_loop (NL::NL::r) [Newline] tl

    (* [ *)
    | _, (Obracket as t)::tl ->
      begin match maybe_link rev_main_loop r previous tl with
      | Some(r, p, l) -> main_loop r p l
      | None ->
        match maybe_reference rev_main_loop rc r previous tl with
        | Some(r, p, l) -> main_loop r p l
        | None ->
          begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop (Text(string_of_t t)::r) [t] tl
          | Some(r, p, l) -> main_loop r p l
          end
      end

    (* img *)
    | _, (Exclamation|Exclamations _ as t)
      ::Obracket::Cbracket::Oparenthesis::tl ->
      (* image insertion with no "alt" *)
      (* ![](/path/to/img.jpg) *)
      (try
         begin
           let b, tl = read_until_cparenth ~no_nl:false tl in
           (* new lines there are allowed *)
           let r (* updated result *) = match t with
             | Exclamations 0 -> Text "!" :: r
             | Exclamations n -> Text(String.make (n+1) '!') :: r
             | _ -> r in
           match
             try Some(read_until_space ~no_nl:true b)
             with Premature_ending -> None
           with
           | Some(url, tls) ->
             let title, should_be_empty_list =
               read_until_dq (snd (read_until_dq tls)) in
             main_loop (Img("", string_of_tl url, string_of_tl title) :: r)
               [Cparenthesis] tl
           | None ->
             main_loop (Img("", string_of_tl b, "") :: r)
               [Cparenthesis] tl
         end
       with
       | NL_exception ->
         begin match maybe_extension extensions r previous lexemes with
         | None -> main_loop (Text(string_of_t t)::r) [t] tl
         | Some(r, p, l) -> main_loop r p l
         end
      )

    (* img ref *)
    | _, (Exclamation|Exclamations _ as t)
      ::Obracket::Cbracket::Obracket::tl ->
      (* ref image insertion with no "alt" *)
      (* ![][ref] *)
      (try
         let id, tl = read_until_cbracket ~no_nl:true tl in
         let r = match t with
           | Exclamations 0 -> Text "!" :: r
           | Exclamations n -> Text(String.make (n+1) '!') :: r
           | _ -> r in
         let r = Img_ref(rc, string_of_tl id, "") :: r in
         main_loop r [Cbracket] tl
       with NL_exception ->
         begin match maybe_extension extensions r previous lexemes with
         | None -> main_loop (Text(string_of_t t)::r) [t] tl
         | Some(r, p, l) -> main_loop r p l
         end
      )


    (* img *)
    | _, (Exclamation|Exclamations _ as t)::Obracket::tl ->
      (* image insertion with "alt" *)
      (* ![Alt text](/path/to/img.jpg "Optional title") *)
      (try
         match read_until_cbracket tl with
         | alt, Oparenthesis::ntl ->
           (try
              let alt = string_of_tl alt in
              let path_title, rest = read_until_cparenth
                ~no_nl:false ntl in
              let path, title = try read_until_space
                                      ~no_nl:true path_title
                with Premature_ending -> path_title, [] in
              let title, nothing =
                if title <> [] then
                  read_until_dq (snd(read_until_dq title))
                else [], [] in
              if nothing <> [] then
                raise NL_exception; (* caught right below *)
              let r =
                match t with
                | Exclamations 0 -> Text "!" :: r
                | Exclamations n -> Text(String.make (n+1) '!') :: r
                | _ -> r in
              let r = Img(alt, string_of_tl path, string_of_tl title)
                :: r in
              main_loop r [Cparenthesis] rest
            with
            | NL_exception
            (* if NL_exception was raised, then fall back to "text" *)
            | Premature_ending ->
              begin match maybe_extension extensions r previous lexemes with
              | None -> main_loop (Text(string_of_t t)::r) [t] tl
              | Some(r, p, l) -> main_loop r p l
              end
           )
         | alt, Obracket::Word(id)::Cbracket::ntl
         | alt, Obracket::(Space|Spaces _)::Word(id)::Cbracket::ntl
         | alt, Obracket::(Space|Spaces _)::Word(id)::(Space|Spaces _)
           ::Cbracket::ntl
         | alt, Obracket::Word(id)::(Space|Spaces _)::Cbracket::ntl ->
           main_loop (Img_ref(rc, id, string_of_tl alt)::r) [Cbracket] ntl
         | alt, Obracket::((Newline|Space|Spaces _|Word _|Number _)::_
                              as ntl) ->
           (try
              match read_until_cbracket ~no_nl:false ntl with
              | [], rest -> raise Premature_ending
              | id, rest ->
                main_loop (Img_ref(rc, string_of_tl id, string_of_tl alt)
                           ::r) [Cbracket] rest
            with
            | Premature_ending
            | NL_exception ->
              begin match maybe_extension extensions r previous lexemes with
              | None -> main_loop (Text(string_of_t t)::r) [t] tl
              | Some(r, p, l) -> main_loop r p l
              end
           )
         | _ ->
           begin match maybe_extension extensions r previous lexemes with
           | None -> main_loop (Text(string_of_t t)::r) [t] tl
           | Some(r, p, l) -> main_loop r p l
           end
       with
       | Premature_ending ->
         begin match maybe_extension extensions r previous lexemes with
         | None -> main_loop (Text(string_of_t t)::r) [t] tl
         | Some(r, p, l) -> main_loop r p l
         end
      )

    | _,
      (At|Bar|Caret|Cbrace|Colon|Comma|Cparenthesis|Cbracket|Dollar
          |Dot|Doublequote|Exclamation|Equal|Minus|Obrace|Oparenthesis
          |Percent|Plus|Question|Quote|Semicolon|Slash|Tab|Tilde|Lessthan
          |Greaterthan as t)::tl
      ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t t)::r) [t] tl
      | Some(r, p, l) -> main_loop r p l
      end
    | _, (Number _  as t):: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t t)::r) [t] tl
      | Some(r, p, l) -> main_loop r p l
      end

    (* generated code: *)
    | _, Ats(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t At)::r) [At] (At::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Ats(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t At)::r) [At] (Ats(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Bars(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Bar)::r) [Bar] (Bar::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Bars(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Bar)::r) [Bar] (Bars(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Carets(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Caret)::r) [Caret] (Caret::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Carets(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Caret)::r) [Caret] (Carets(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Cbraces(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Cbrace)::r) [Cbrace] (Cbrace::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Cbraces(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Cbrace)::r) [Cbrace] (Cbraces(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Cbrackets(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Cbracket)::r) [Cbracket] (Cbracket::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Cbrackets(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Cbracket)::r) [Cbracket] (Cbrackets(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Colons(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Colon)::r) [Colon] (Colon::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Colons(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Colon)::r) [Colon] (Colons(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Commas(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Comma)::r) [Comma] (Comma::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Commas(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Comma)::r) [Comma] (Commas(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Cparenthesiss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Cparenthesis)::r) [Cparenthesis] (Cparenthesis::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Cparenthesiss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Cparenthesis)::r) [Cparenthesis] (Cparenthesiss(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Dollars(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Dollar)::r) [Dollar] (Dollar::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Dollars(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Dollar)::r) [Dollar] (Dollars(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Dots(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Dot)::r) [Dot] (Dot::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Dots(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Dot)::r) [Dot] (Dots(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Doublequotes(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Doublequote)::r) [Doublequote] (Doublequote::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Doublequotes(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Doublequote)::r) [Doublequote] (Doublequotes(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Equals(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Equal)::r) [Equal] (Equal::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Equals(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Equal)::r) [Equal] (Equals(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Exclamations(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Exclamation)::r) [Exclamation] (Exclamation::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Exclamations(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Exclamation)::r) [Exclamation] (Exclamations(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Greaterthans(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Greaterthan)::r) [Greaterthan] (Greaterthan::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Greaterthans(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Greaterthan)::r) [Greaterthan] (Greaterthans(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Lessthans(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Lessthan)::r) [Lessthan] (Lessthan::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Lessthans(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Lessthan)::r) [Lessthan] (Lessthans(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Minuss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Minus)::r) [Minus] (Minus::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Minuss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Minus)::r) [Minus] (Minuss(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Obraces(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Obrace)::r) [Obrace] (Obrace::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Obraces(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Obrace)::r) [Obrace] (Obraces(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Obrackets(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Obracket)::r) [Obracket] (Obracket::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Obrackets(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Obracket)::r) [Obracket] (Obrackets(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Oparenthesiss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Oparenthesis)::r) [Oparenthesis] (Oparenthesis::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Oparenthesiss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Oparenthesis)::r) [Oparenthesis] (Oparenthesiss(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Percents(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Percent)::r) [Percent] (Percent::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Percents(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Percent)::r) [Percent] (Percents(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Pluss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Plus)::r) [Plus] (Plus::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Pluss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Plus)::r) [Plus] (Pluss(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Questions(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Question)::r) [Question] (Question::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Questions(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Question)::r) [Question] (Questions(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Quotes(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Quote)::r) [Quote] (Quote::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Quotes(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Quote)::r) [Quote] (Quotes(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Semicolons(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Semicolon)::r) [Semicolon] (Semicolon::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Semicolons(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Semicolon)::r) [Semicolon] (Semicolons(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Slashs(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Slash)::r) [Slash] (Slash::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Slashs(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Slash)::r) [Slash] (Slashs(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Stars(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Star)::r) [Star] (Stars(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Tabs(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Tab)::r) [Tab] (Tab::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Tabs(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Tab)::r) [Tab] (Tabs(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Tildes(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Tilde)::r) [Tilde] (Tilde::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Tildes(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Tilde)::r) [Tilde] (Tildes(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
    | _, Underscores(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop (Text(string_of_t Underscore)::r) [Underscore] (Underscores(n-1)::tl)
      | Some(r, p, l) -> main_loop r p l
      end
  (* /generated code *)


  and rev_main_loop (r: t) (previous:Omd_representation.tok list)
      (lexemes:Omd_representation.tok list) =
    List.rev (main_loop r previous lexemes)

  in
  rev_main_loop [] [] (tag_setext rev_main_loop lexemes)


let parse ?(extensions=[]) lexemes =
  main_parse extensions lexemes

(******************************************************************************)

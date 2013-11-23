(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)
type r = Omd_representation.t
(** accumulator (beware, reversed tokens) *)

and p = Omd_representation.tok list
(** context information: previous elements *)

and l = Omd_representation.tok list
(** tokens to parse *)

and main_loop =
  r -> (* accumulator (beware, reversed tokens) *)
  p -> (* info: previous elements *)
  l -> (* tokens to parse *)
  Omd_representation.t (* final result *)
(** most important loop *)


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
  (* list extracted from: http://www.w3.org/TR/html4/charset.html *)
  [ "AElig";  "Aacute";  "Acirc";  "Agrave"; "Alpha";  "Aring";  "Atilde";
    "Auml"; "Beta";  "Ccedil"; "Chi"; "Dagger";  "Delta"; "ETH"; "Eacute";
    "Ecirc";  "Egrave";  "Epsilon";   "Eta";  "Euml";  "Gamma";  "Iacute";
    "Icirc"; "Igrave"; "Iota";  "Iuml"; "Kappa"; "Lambda"; "Mu"; "Ntilde";
    "Nu";  "OElig";  "Oacute";   "Ocirc";  "Ograve";  "Omega";  "Omicron";
    "Oslash";  "Otilde";  "Ouml";  "Phi";  "Pi";  "Prime";  "Psi";  "Rho";
    "Scaron";  "Sigma";   "THORN";  "Tau";  "Theta";   "Uacute";  "Ucirc";
    "Ugrave"; "Upsilon"; "Uuml"; "Xi"; "Yacute"; "Yuml"; "Zeta"; "aacute";
    "acirc"; "acute"; "aelig"; "agrave"; "alefsym"; "alpha"; "amp"; "and";
    "ang"; "aring"; "asymp";  "atilde"; "auml"; "bdquo"; "beta"; "brvbar";
    "bull";  "cap";  "ccedil"; "cedil";  "cent";  "chi"; "circ";  "clubs";
    "cong";  "copy"; "crarr"; "cup";  "curren"; "dArr";  "dagger"; "darr";
    "deg";  "delta";  "diams";   "divide";  "eacute";  "ecirc";  "egrave";
    "empty";  "emsp"; "ensp";  "epsilon"; "equiv";  "eta";  "eth"; "euml";
    "euro";  "exist";  "fnof";  "forall";  "frac12";  "frac14";  "frac34";
    "frasl";  "gamma";  "ge"; "gt";  "hArr";  "harr"; "hearts";  "hellip";
    "iacute"; "icirc"; "iexcl"; "igrave"; "image"; "infin"; "int"; "iota";
    "iquest"; "isin"; "iuml";  "kappa"; "lArr"; "lambda"; "lang"; "laquo";
    "larr";  "lceil";  "ldquo"; "le";  "lfloor";  "lowast"; "loz";  "lrm";
    "lsaquo"; "lsquo"; "lt";  "macr"; "mdash"; "micro"; "middot"; "minus";
    "mu"; "nabla";  "nbsp"; "ndash";  "ne"; "ni"; "not";  "notin"; "nsub";
    "ntilde";  "nu";   "oacute";  "ocirc";  "oelig";   "ograve";  "oline";
    "omega"; "omicron"; "oplus"; "or"; "ordf"; "ordm"; "oslash"; "otilde";
    "otimes";  "ouml";  "para";  "part";  "permil"; "perp";  "phi";  "pi";
    "piv";  "plusmn";  "pound"; "prime";  "prod";  "prop"; "psi";  "quot";
    "rArr";  "radic"; "rang"; "raquo";  "rarr"; "rceil";  "rdquo"; "real";
    "reg"; "rfloor";  "rho"; "rlm"; "rsaquo";  "rsquo"; "sbquo"; "scaron";
    "sdot";  "sect";  "shy"; "sigma";  "sigmaf";  "sim"; "spades";  "sub";
    "sube"; "sum"; "sup"; "sup1";  "sup2"; "sup3"; "supe"; "szlig"; "tau";
    "there4";  "theta"; "thetasym";  "thinsp"; "thorn";  "tilde"; "times";
    "trade"; "uArr"; "uacute";  "uarr"; "ucirc"; "ugrave"; "uml"; "upsih";
    "upsilon";  "uuml"; "weierp"; "xi";  "yacute"; "yen";  "yuml"; "zeta";
    "zwj"; "zwnj"; ]


(** set of known inline HTML tags *)
let inline_htmltags_set = StringSet.of_list
  (* from https://developer.mozilla.org/en-US/docs/HTML/Inline_elements *)
  [ "b";"big";"i";"small";"tt";
    "abbr";"acronym";"cite";"code";"dfn";"em";"kbd";"strong";"samp";"var";
    "a";"bdo";"br";"img";"map";"object";"q";"span";"sub";"sup";
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
       ;"h2";"h3";"h4";"h5";"h6"
       ;"h1";"header";"hr";"i";"iframe";"img";"input";"ins";"kbd"
       ;"keygen";"label";"legend";"li";"map";"mark";"menu";"meter";"nav"
       ;"noframes";"noscript";"object";"ol";"optgroup";"option";"output"
       ;"p";"param";"pre";"progress";"q";"rp";"rt";"ruby";"s";"samp"
       ;"script";"section";"select";"small";"source";"span";"strike"
       ;"strong";"style";"sub";"summary";"sup";"table";"tbody";"td"
       ;"textarea";"tfoot";"th";"thead";"time";"tr";"track";"tt";"u"
       ;"ul";"var";"video";"wbr"
     ])


(** This functions fixes bad lexing trees, which may be built when
    extraction a portion of another lexing tree. *)
let fix l =
  let rec loop accu = function
(* code to generate what follows...
List.iter (fun e ->
Printf.printf "
| %s::%s::tl -> if trackfix then eprintf \"%s 1\\n%!\"; loop accu (%ss 0::tl)
| %ss n::%s::tl -> if trackfix then eprintf \"%s 2\\n%!\"; loop accu (%ss(n+1)::tl)
| %s::%ss n::tl -> if trackfix then eprintf \"%s 3\\n%!\"; loop accu (%ss(n+1)::tl)
| %ss a::%ss b::tl -> if trackfix then eprintf \"%s 4\\n%!\"; loop accu (%ss(a+b+2)::tl)"
e e e e e e e e e e e e e e e e)
["Ampersand"; "At"; "Backquote"; "Backslash"; "Bar"; "Caret"; "Cbrace"; "Colon"; "Comma"; "Cparenthesis"; "Cbracket"; "Dollar"; "Dot"; "Doublequote"; "Exclamation"; "Equal"; "Greaterthan"; "Hash"; "Lessthan"; "Minus"; "Newline"; "Obrace"; "Oparenthesis"; "Obracket"; "Percent"; "Plus"; "Question"; "Quote"; "Semicolon"; "Slash"; "Space"; "Star"; "Tab"; "Tilde"; "Underscore"];
print_string "| x::tl -> loop (x::accu) tl\n| [] -> List.rev accu\n"; *)
      | Ampersand::Ampersand::tl -> if trackfix then eprintf "Ampersand 1\n"; loop accu (Ampersands 0::tl)
      | Ampersands n::Ampersand::tl -> if trackfix then eprintf "Ampersand 2\n"; loop accu (Ampersands(n+1)::tl)
      | Ampersand::Ampersands n::tl -> if trackfix then eprintf "Ampersand 3\n"; loop accu (Ampersands(n+1)::tl)
      | Ampersands a::Ampersands b::tl -> if trackfix then eprintf "Ampersand 4\n"; loop accu (Ampersands(a+b+2)::tl)
      | At::At::tl -> if trackfix then eprintf "At 1\n"; loop accu (Ats 0::tl)
      | Ats n::At::tl -> if trackfix then eprintf "At 2\n"; loop accu (Ats(n+1)::tl)
      | At::Ats n::tl -> if trackfix then eprintf "At 3\n"; loop accu (Ats(n+1)::tl)
      | Ats a::Ats b::tl -> if trackfix then eprintf "At 4\n"; loop accu (Ats(a+b+2)::tl)
      | Backquote::Backquote::tl -> if trackfix then eprintf "Backquote 1\n"; loop accu (Backquotes 0::tl)
      | Backquotes n::Backquote::tl -> if trackfix then eprintf "Backquote 2\n"; loop accu (Backquotes(n+1)::tl)
      | Backquote::Backquotes n::tl -> if trackfix then eprintf "Backquote 3\n"; loop accu (Backquotes(n+1)::tl)
      | Backquotes a::Backquotes b::tl -> if trackfix then eprintf "Backquote 4\n"; loop accu (Backquotes(a+b+2)::tl)
      | Backslash::Backslash::tl -> if trackfix then eprintf "Backslash 1\n"; loop accu (Backslashs 0::tl)
      | Backslashs n::Backslash::tl -> if trackfix then eprintf "Backslash 2\n"; loop accu (Backslashs(n+1)::tl)
      | Backslash::Backslashs n::tl -> if trackfix then eprintf "Backslash 3\n"; loop accu (Backslashs(n+1)::tl)
      | Backslashs a::Backslashs b::tl -> if trackfix then eprintf "Backslash 4\n"; loop accu (Backslashs(a+b+2)::tl)
      | Bar::Bar::tl -> if trackfix then eprintf "Bar 1\n"; loop accu (Bars 0::tl)
      | Bars n::Bar::tl -> if trackfix then eprintf "Bar 2\n"; loop accu (Bars(n+1)::tl)
      | Bar::Bars n::tl -> if trackfix then eprintf "Bar 3\n"; loop accu (Bars(n+1)::tl)
      | Bars a::Bars b::tl -> if trackfix then eprintf "Bar 4\n"; loop accu (Bars(a+b+2)::tl)
      | Caret::Caret::tl -> if trackfix then eprintf "Caret 1\n"; loop accu (Carets 0::tl)
      | Carets n::Caret::tl -> if trackfix then eprintf "Caret 2\n"; loop accu (Carets(n+1)::tl)
      | Caret::Carets n::tl -> if trackfix then eprintf "Caret 3\n"; loop accu (Carets(n+1)::tl)
      | Carets a::Carets b::tl -> if trackfix then eprintf "Caret 4\n"; loop accu (Carets(a+b+2)::tl)
      | Cbrace::Cbrace::tl -> if trackfix then eprintf "Cbrace 1\n"; loop accu (Cbraces 0::tl)
      | Cbraces n::Cbrace::tl -> if trackfix then eprintf "Cbrace 2\n"; loop accu (Cbraces(n+1)::tl)
      | Cbrace::Cbraces n::tl -> if trackfix then eprintf "Cbrace 3\n"; loop accu (Cbraces(n+1)::tl)
      | Cbraces a::Cbraces b::tl -> if trackfix then eprintf "Cbrace 4\n"; loop accu (Cbraces(a+b+2)::tl)
      | Colon::Colon::tl -> if trackfix then eprintf "Colon 1\n"; loop accu (Colons 0::tl)
      | Colons n::Colon::tl -> if trackfix then eprintf "Colon 2\n"; loop accu (Colons(n+1)::tl)
      | Colon::Colons n::tl -> if trackfix then eprintf "Colon 3\n"; loop accu (Colons(n+1)::tl)
      | Colons a::Colons b::tl -> if trackfix then eprintf "Colon 4\n"; loop accu (Colons(a+b+2)::tl)
      | Comma::Comma::tl -> if trackfix then eprintf "Comma 1\n"; loop accu (Commas 0::tl)
      | Commas n::Comma::tl -> if trackfix then eprintf "Comma 2\n"; loop accu (Commas(n+1)::tl)
      | Comma::Commas n::tl -> if trackfix then eprintf "Comma 3\n"; loop accu (Commas(n+1)::tl)
      | Commas a::Commas b::tl -> if trackfix then eprintf "Comma 4\n"; loop accu (Commas(a+b+2)::tl)
      | Cparenthesis::Cparenthesis::tl -> if trackfix then eprintf "Cparenthesis 1\n"; loop accu (Cparenthesiss 0::tl)
      | Cparenthesiss n::Cparenthesis::tl -> if trackfix then eprintf "Cparenthesis 2\n"; loop accu (Cparenthesiss(n+1)::tl)
      | Cparenthesis::Cparenthesiss n::tl -> if trackfix then eprintf "Cparenthesis 3\n"; loop accu (Cparenthesiss(n+1)::tl)
      | Cparenthesiss a::Cparenthesiss b::tl -> if trackfix then eprintf "Cparenthesis 4\n"; loop accu (Cparenthesiss(a+b+2)::tl)
      | Cbracket::Cbracket::tl -> if trackfix then eprintf "Cbracket 1\n"; loop accu (Cbrackets 0::tl)
      | Cbrackets n::Cbracket::tl -> if trackfix then eprintf "Cbracket 2\n"; loop accu (Cbrackets(n+1)::tl)
      | Cbracket::Cbrackets n::tl -> if trackfix then eprintf "Cbracket 3\n"; loop accu (Cbrackets(n+1)::tl)
      | Cbrackets a::Cbrackets b::tl -> if trackfix then eprintf "Cbracket 4\n"; loop accu (Cbrackets(a+b+2)::tl)
      | Dollar::Dollar::tl -> if trackfix then eprintf "Dollar 1\n"; loop accu (Dollars 0::tl)
      | Dollars n::Dollar::tl -> if trackfix then eprintf "Dollar 2\n"; loop accu (Dollars(n+1)::tl)
      | Dollar::Dollars n::tl -> if trackfix then eprintf "Dollar 3\n"; loop accu (Dollars(n+1)::tl)
      | Dollars a::Dollars b::tl -> if trackfix then eprintf "Dollar 4\n"; loop accu (Dollars(a+b+2)::tl)
      | Dot::Dot::tl -> if trackfix then eprintf "Dot 1\n"; loop accu (Dots 0::tl)
      | Dots n::Dot::tl -> if trackfix then eprintf "Dot 2\n"; loop accu (Dots(n+1)::tl)
      | Dot::Dots n::tl -> if trackfix then eprintf "Dot 3\n"; loop accu (Dots(n+1)::tl)
      | Dots a::Dots b::tl -> if trackfix then eprintf "Dot 4\n"; loop accu (Dots(a+b+2)::tl)
      | Doublequote::Doublequote::tl -> if trackfix then eprintf "Doublequote 1\n"; loop accu (Doublequotes 0::tl)
      | Doublequotes n::Doublequote::tl -> if trackfix then eprintf "Doublequote 2\n"; loop accu (Doublequotes(n+1)::tl)
      | Doublequote::Doublequotes n::tl -> if trackfix then eprintf "Doublequote 3\n"; loop accu (Doublequotes(n+1)::tl)
      | Doublequotes a::Doublequotes b::tl -> if trackfix then eprintf "Doublequote 4\n"; loop accu (Doublequotes(a+b+2)::tl)
      | Exclamation::Exclamation::tl -> if trackfix then eprintf "Exclamation 1\n"; loop accu (Exclamations 0::tl)
      | Exclamations n::Exclamation::tl -> if trackfix then eprintf "Exclamation 2\n"; loop accu (Exclamations(n+1)::tl)
      | Exclamation::Exclamations n::tl -> if trackfix then eprintf "Exclamation 3\n"; loop accu (Exclamations(n+1)::tl)
      | Exclamations a::Exclamations b::tl -> if trackfix then eprintf "Exclamation 4\n"; loop accu (Exclamations(a+b+2)::tl)
      | Equal::Equal::tl -> if trackfix then eprintf "Equal 1\n"; loop accu (Equals 0::tl)
      | Equals n::Equal::tl -> if trackfix then eprintf "Equal 2\n"; loop accu (Equals(n+1)::tl)
      | Equal::Equals n::tl -> if trackfix then eprintf "Equal 3\n"; loop accu (Equals(n+1)::tl)
      | Equals a::Equals b::tl -> if trackfix then eprintf "Equal 4\n"; loop accu (Equals(a+b+2)::tl)
      | Greaterthan::Greaterthan::tl -> if trackfix then eprintf "Greaterthan 1\n"; loop accu (Greaterthans 0::tl)
      | Greaterthans n::Greaterthan::tl -> if trackfix then eprintf "Greaterthan 2\n"; loop accu (Greaterthans(n+1)::tl)
      | Greaterthan::Greaterthans n::tl -> if trackfix then eprintf "Greaterthan 3\n"; loop accu (Greaterthans(n+1)::tl)
      | Greaterthans a::Greaterthans b::tl -> if trackfix then eprintf "Greaterthan 4\n"; loop accu (Greaterthans(a+b+2)::tl)
      | Hash::Hash::tl -> if trackfix then eprintf "Hash 1\n"; loop accu (Hashs 0::tl)
      | Hashs n::Hash::tl -> if trackfix then eprintf "Hash 2\n"; loop accu (Hashs(n+1)::tl)
      | Hash::Hashs n::tl -> if trackfix then eprintf "Hash 3\n"; loop accu (Hashs(n+1)::tl)
      | Hashs a::Hashs b::tl -> if trackfix then eprintf "Hash 4\n"; loop accu (Hashs(a+b+2)::tl)
      | Lessthan::Lessthan::tl -> if trackfix then eprintf "Lessthan 1\n"; loop accu (Lessthans 0::tl)
      | Lessthans n::Lessthan::tl -> if trackfix then eprintf "Lessthan 2\n"; loop accu (Lessthans(n+1)::tl)
      | Lessthan::Lessthans n::tl -> if trackfix then eprintf "Lessthan 3\n"; loop accu (Lessthans(n+1)::tl)
      | Lessthans a::Lessthans b::tl -> if trackfix then eprintf "Lessthan 4\n"; loop accu (Lessthans(a+b+2)::tl)
      | Minus::Minus::tl -> if trackfix then eprintf "Minus 1\n"; loop accu (Minuss 0::tl)
      | Minuss n::Minus::tl -> if trackfix then eprintf "Minus 2\n"; loop accu (Minuss(n+1)::tl)
      | Minus::Minuss n::tl -> if trackfix then eprintf "Minus 3\n"; loop accu (Minuss(n+1)::tl)
      | Minuss a::Minuss b::tl -> if trackfix then eprintf "Minus 4\n"; loop accu (Minuss(a+b+2)::tl)
      | Newline::Newline::tl -> if trackfix then eprintf "Newline 1\n"; loop accu (Newlines 0::tl)
      | Newlines n::Newline::tl -> if trackfix then eprintf "Newline 2\n"; loop accu (Newlines(n+1)::tl)
      | Newline::Newlines n::tl -> if trackfix then eprintf "Newline 3\n"; loop accu (Newlines(n+1)::tl)
      | Newlines a::Newlines b::tl -> if trackfix then eprintf "Newline 4\n"; loop accu (Newlines(a+b+2)::tl)
      | Obrace::Obrace::tl -> if trackfix then eprintf "Obrace 1\n"; loop accu (Obraces 0::tl)
      | Obraces n::Obrace::tl -> if trackfix then eprintf "Obrace 2\n"; loop accu (Obraces(n+1)::tl)
      | Obrace::Obraces n::tl -> if trackfix then eprintf "Obrace 3\n"; loop accu (Obraces(n+1)::tl)
      | Obraces a::Obraces b::tl -> if trackfix then eprintf "Obrace 4\n"; loop accu (Obraces(a+b+2)::tl)
      | Oparenthesis::Oparenthesis::tl -> if trackfix then eprintf "Oparenthesis 1\n"; loop accu (Oparenthesiss 0::tl)
      | Oparenthesiss n::Oparenthesis::tl -> if trackfix then eprintf "Oparenthesis 2\n"; loop accu (Oparenthesiss(n+1)::tl)
      | Oparenthesis::Oparenthesiss n::tl -> if trackfix then eprintf "Oparenthesis 3\n"; loop accu (Oparenthesiss(n+1)::tl)
      | Oparenthesiss a::Oparenthesiss b::tl -> if trackfix then eprintf "Oparenthesis 4\n"; loop accu (Oparenthesiss(a+b+2)::tl)
      | Obracket::Obracket::tl -> if trackfix then eprintf "Obracket 1\n"; loop accu (Obrackets 0::tl)
      | Obrackets n::Obracket::tl -> if trackfix then eprintf "Obracket 2\n"; loop accu (Obrackets(n+1)::tl)
      | Obracket::Obrackets n::tl -> if trackfix then eprintf "Obracket 3\n"; loop accu (Obrackets(n+1)::tl)
      | Obrackets a::Obrackets b::tl -> if trackfix then eprintf "Obracket 4\n"; loop accu (Obrackets(a+b+2)::tl)
      | Percent::Percent::tl -> if trackfix then eprintf "Percent 1\n"; loop accu (Percents 0::tl)
      | Percents n::Percent::tl -> if trackfix then eprintf "Percent 2\n"; loop accu (Percents(n+1)::tl)
      | Percent::Percents n::tl -> if trackfix then eprintf "Percent 3\n"; loop accu (Percents(n+1)::tl)
      | Percents a::Percents b::tl -> if trackfix then eprintf "Percent 4\n"; loop accu (Percents(a+b+2)::tl)
      | Plus::Plus::tl -> if trackfix then eprintf "Plus 1\n"; loop accu (Pluss 0::tl)
      | Pluss n::Plus::tl -> if trackfix then eprintf "Plus 2\n"; loop accu (Pluss(n+1)::tl)
      | Plus::Pluss n::tl -> if trackfix then eprintf "Plus 3\n"; loop accu (Pluss(n+1)::tl)
      | Pluss a::Pluss b::tl -> if trackfix then eprintf "Plus 4\n"; loop accu (Pluss(a+b+2)::tl)
      | Question::Question::tl -> if trackfix then eprintf "Question 1\n"; loop accu (Questions 0::tl)
      | Questions n::Question::tl -> if trackfix then eprintf "Question 2\n"; loop accu (Questions(n+1)::tl)
      | Question::Questions n::tl -> if trackfix then eprintf "Question 3\n"; loop accu (Questions(n+1)::tl)
      | Questions a::Questions b::tl -> if trackfix then eprintf "Question 4\n"; loop accu (Questions(a+b+2)::tl)
      | Quote::Quote::tl -> if trackfix then eprintf "Quote 1\n"; loop accu (Quotes 0::tl)
      | Quotes n::Quote::tl -> if trackfix then eprintf "Quote 2\n"; loop accu (Quotes(n+1)::tl)
      | Quote::Quotes n::tl -> if trackfix then eprintf "Quote 3\n"; loop accu (Quotes(n+1)::tl)
      | Quotes a::Quotes b::tl -> if trackfix then eprintf "Quote 4\n"; loop accu (Quotes(a+b+2)::tl)
      | Semicolon::Semicolon::tl -> if trackfix then eprintf "Semicolon 1\n"; loop accu (Semicolons 0::tl)
      | Semicolons n::Semicolon::tl -> if trackfix then eprintf "Semicolon 2\n"; loop accu (Semicolons(n+1)::tl)
      | Semicolon::Semicolons n::tl -> if trackfix then eprintf "Semicolon 3\n"; loop accu (Semicolons(n+1)::tl)
      | Semicolons a::Semicolons b::tl -> if trackfix then eprintf "Semicolon 4\n"; loop accu (Semicolons(a+b+2)::tl)
      | Slash::Slash::tl -> if trackfix then eprintf "Slash 1\n"; loop accu (Slashs 0::tl)
      | Slashs n::Slash::tl -> if trackfix then eprintf "Slash 2\n"; loop accu (Slashs(n+1)::tl)
      | Slash::Slashs n::tl -> if trackfix then eprintf "Slash 3\n"; loop accu (Slashs(n+1)::tl)
      | Slashs a::Slashs b::tl -> if trackfix then eprintf "Slash 4\n"; loop accu (Slashs(a+b+2)::tl)
      | Space::Space::tl -> if trackfix then eprintf "Space 1\n"; loop accu (Spaces 0::tl)
      | Spaces n::Space::tl -> if trackfix then eprintf "Space 2\n"; loop accu (Spaces(n+1)::tl)
      | Space::Spaces n::tl -> if trackfix then eprintf "Space 3\n"; loop accu (Spaces(n+1)::tl)
      | Spaces a::Spaces b::tl -> if trackfix then eprintf "Space 4\n"; loop accu (Spaces(a+b+2)::tl)
      | Star::Star::tl -> if trackfix then eprintf "Star 1\n"; loop accu (Stars 0::tl)
      | Stars n::Star::tl -> if trackfix then eprintf "Star 2\n"; loop accu (Stars(n+1)::tl)
      | Star::Stars n::tl -> if trackfix then eprintf "Star 3\n"; loop accu (Stars(n+1)::tl)
      | Stars a::Stars b::tl -> if trackfix then eprintf "Star 4\n"; loop accu (Stars(a+b+2)::tl)
      | Tab::Tab::tl -> if trackfix then eprintf "Tab 1\n"; loop accu (Tabs 0::tl)
      | Tabs n::Tab::tl -> if trackfix then eprintf "Tab 2\n"; loop accu (Tabs(n+1)::tl)
      | Tab::Tabs n::tl -> if trackfix then eprintf "Tab 3\n"; loop accu (Tabs(n+1)::tl)
      | Tabs a::Tabs b::tl -> if trackfix then eprintf "Tab 4\n"; loop accu (Tabs(a+b+2)::tl)
      | Tilde::Tilde::tl -> if trackfix then eprintf "Tilde 1\n"; loop accu (Tildes 0::tl)
      | Tildes n::Tilde::tl -> if trackfix then eprintf "Tilde 2\n"; loop accu (Tildes(n+1)::tl)
      | Tilde::Tildes n::tl -> if trackfix then eprintf "Tilde 3\n"; loop accu (Tildes(n+1)::tl)
      | Tildes a::Tildes b::tl -> if trackfix then eprintf "Tilde 4\n"; loop accu (Tildes(a+b+2)::tl)
      | Underscore::Underscore::tl -> if trackfix then eprintf "Underscore 1\n"; loop accu (Underscores 0::tl)
      | Underscores n::Underscore::tl -> if trackfix then eprintf "Underscore 2\n"; loop accu (Underscores(n+1)::tl)
      | Underscore::Underscores n::tl -> if trackfix then eprintf "Underscore 3\n"; loop accu (Underscores(n+1)::tl)
      | Underscores a::Underscores b::tl -> if trackfix then eprintf "Underscore 4\n"; loop accu (Underscores(a+b+2)::tl)| x::tl -> loop (x::accu) tl
      | [] -> List.rev accu
    in
    loop [] l

(** [assert_well_formed] is a developer's function that helps to track badly constructed token lists.
    This function has an effect only if [trackfix] is [true].
 *)
let assert_well_formed (l:tok list) : unit =
  if trackfix then
    let rec equiv l1 l2 = match l1, l2 with
      | [], [] -> true
      | Tag _::tl1, Tag _::tl2-> equiv tl1 tl2
      | e1::tl1, e2::tl2 -> e1 = e2 && equiv tl1 tl2
      | _ -> false
    in
    assert(equiv (fix l) l);
    ()

(** Generate fallback for references. *)
let extract_fallback remains l =
  let rec loop accu = function
    | [] -> string_of_tl (List.rev accu)
    | e::tl as r ->
      if r == remains then
        string_of_tl (List.rev accu)
      else
        loop (e::accu) tl
  in loop [] l


let unindent_rev n lexemes =
  if debug then eprintf "CALL: Omd_parser.unindent_rev\n%!";
  assert_well_formed lexemes;
  let rec loop accu cl = function
    | Newline::Space::tl as l ->
      if n = 1 then
        loop (Newline::cl@accu) [] tl
      else
        (cl@accu), l
    | Newline::Spaces(0)::tl as l ->
      if n = 1 then
        loop (Newline::cl@accu) [Space] tl
      else if n = 2 then
        loop (Newline::cl@accu) [] tl
      else
        (cl@accu), l
    | Newline::Spaces(s)::tl as l ->
      assert(s>0);
      if s+2 = n then
        loop (Newline::cl@accu) [] tl
      else if s+2 > n then
        loop (Newline::cl@accu) (Omd_lexer.lex(String.make (s+2-n) ' ')) tl
      else
        (cl@accu), l
    | Newlines(_)::_ as l ->
      (cl@accu), l
    | Newline::_ as l ->
      (cl@accu), l
    | e::tl ->
      loop accu (e::cl) tl
    | [] as l ->
      (cl@accu), l
  in
  match loop [] [] lexemes with
  | [], right -> [], right
  | l, right ->
      assert_well_formed l;
      l, right

let unindent n lexemes =
  if debug then eprintf "CALL: Omd_parser.unindent\n%!";
  let fst, snd = unindent_rev n lexemes in
    List.rev fst, snd

let rec is_blank = function
  | (Space | Spaces _ | Newline | Newlines _) :: tl ->
      is_blank tl
  | [] -> true
  | _ -> false

let semph_or_bold (n:int) (l:Omd_representation.tok list) =
  (* FIXME: use rpl call/return convention *)
  assert_well_formed l;
  assert (n>0 && n<4);
  match
    fsplit
      ~excl:(function Newlines _ :: _ -> true | _ -> false)
      ~f:(function
            | Backslash::Star::tl ->
                Continue_with([Star;Backslash],tl)
            | Backslash::Stars 0::tl ->
                Continue_with([Star;Backslash],Star::tl)
            | Backslash::Stars n::tl ->
                Continue_with([Star;Backslash],Stars(n-1)::tl)
            | (Backslashs b as x)::Star::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],Star::tl)
                else
                  Continue_with([Star;x],tl)
            | (Backslashs b as x)::(Stars 0 as s)::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],s::tl)
                else
                  Continue_with([Star;x],Star::tl)
            | (Backslashs b as x)::(Stars n as s)::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],s::tl)
                else
                  Continue_with([Star;x],Stars(n-1)::tl)
            | (Space|Spaces _ as x)::(Star|Stars _ as s)::tl ->
                Continue_with([s;x],tl)
            | (Star|Stars _ as s)::tl ->
                if String.length(string_of_t s) = n then
                  Split([],tl)
                else
                  Continue
            | _ -> Continue)
      l
  with
    | None ->
        None
    | Some(left,right) ->
        if is_blank left then None else Some(left,right)

let sm_uemph_or_bold (n:int) (l:Omd_representation.tok list) =
  assert_well_formed l;
  (* FIXME: use rpl call/return convention *)
  assert (n>0 && n<4);
  match
    fsplit
      ~excl:(function Newlines _ :: _ -> true | _ -> false)
      ~f:(function
            | Backslash::Underscore::tl ->
                Continue_with([Underscore;Backslash],tl)
            | Backslash::Underscores 0::tl ->
                Continue_with([Underscore;Backslash],Underscore::tl)
            | Backslash::Underscores n::tl ->
                Continue_with([Underscore;Backslash],Underscores(n-1)::tl)
            | (Backslashs b as x)::Underscore::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],Underscore::tl)
                else
                  Continue_with([Underscore;x],tl)
            | (Backslashs b as x)::(Underscores 0 as s)::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],s::tl)
                else
                  Continue_with([Underscore;x],Underscore::tl)
            | (Backslashs b as x)::(Underscores n as s)::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],s::tl)
                else
                  Continue_with([Underscore;x],Underscores(n-1)::tl)
            | (Space|Spaces _ as x)::(Underscore|Underscores _ as s)::tl ->
                Continue_with([s;x],tl)
            | (Underscore|Underscores _ as s)::tl ->
                if String.length(string_of_t s) = n then
                  Split([],tl)
                else
                  Continue
            | _ -> Continue)
      l
  with
    | None ->
        None
    | Some(left,right) ->
        if is_blank left then None else Some(left,right)


let gh_uemph_or_bold (n:int) (l:Omd_representation.tok list) =
  assert_well_formed l;
  (* FIXME: use rpl call/return convention *)
  assert (n>0 && n<4);
  match
    fsplit
      ~excl:(function Newlines _ :: _ -> true | _ -> false)
      ~f:(function
            | Backslash::Underscore::tl ->
                Continue_with([Underscore;Backslash],tl)
            | Backslash::Underscores 0::tl ->
                Continue_with([Underscore;Backslash],Underscore::tl)
            | Backslash::Underscores n::tl ->
                Continue_with([Underscore;Backslash],Underscores(n-1)::tl)
            | (Backslashs b as x)::Underscore::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],Underscore::tl)
                else
                  Continue_with([Underscore;x],tl)
            | (Backslashs b as x)::(Underscores 0 as s)::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],s::tl)
                else
                  Continue_with([Underscore;x],Underscore::tl)
            | (Backslashs b as x)::(Underscores n as s)::tl ->
                if b mod 2 = 0 then
                  Continue_with([x],s::tl)
                else
                  Continue_with([Underscore;x],Underscores(n-1)::tl)
            | (Space|Spaces _ as x)::(Underscore|Underscores _ as s)::tl ->
                Continue_with([s;x],tl)
            | (Underscore|Underscores _ as s)::(Word _|Number _ as w):: tl ->
                Continue_with([w;s],tl)
            | (Underscore|Underscores _ as s)::tl ->
                if String.length(string_of_t s) = n then
                  Split([],tl)
                else
                  Continue
            | _ -> Continue)
      l
  with
    | None ->
        None
    | Some(left,right) ->
        if is_blank left then None else Some(left,right)


let uemph_or_bold n l =
  assert_well_formed l;
  (* FIXME: use rpl call/return convention *)
  if !gh_uemph_or_bold_style then
    gh_uemph_or_bold n l
  else
    sm_uemph_or_bold n l

let eat_blank =
  eat (function |Space|Spaces _|Newline|Newlines _ -> true| _ -> false)


(* used by tag__maybe_h1 and tag__maybe_h2 *)
let setext_title l =
  assert_well_formed l;
(* val setext_title :
  Omd_representation.tok list ->
  (Omd_representation.tok list * Omd_representation.tok list) option *)
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


let tag__maybe_h1 main_loop =
  Tag(fun r p l ->
    match p with
    | ([]|[Newline|Newlines _]) ->
      begin match setext_title l with
      | None ->
        None
      | Some(title, tl) ->
        let title = H1(main_loop [] [] title) in
        Some((title::r), [Newline], tl)
      end
    | _ ->
      if debug then
        begin
         Printf.eprintf "In tag__maybe_h1, p=%S \
                         and that shouldn't be possible!\n" (string_of_tl p);
         assert false (* -> the tag generator would be broken *)
        end
      else
        None
  )

let tag__maybe_h2 main_loop =
  Tag(fun r p l ->
    match p with
    | ([]|[Newline|Newlines _]) ->
      begin match setext_title l with
      | None ->
        None
      | Some(title, tl) ->
        let title = H2(main_loop [] [] title) in
        Some((title::r), [Newline], tl)
      end
    | _ ->
      if debug then
        begin
         Printf.eprintf "In tag__maybe_h2, p=%S \
                         and that shouldn't be possible!\n" (string_of_tl p);
         assert false (* -> the tag generator would be broken *)
        end
      else
        None
  )

let tag__md md = (* [md] should be in reverse *)
  Tag(fun r p l -> Some(md@r, [], l))

(* Let's tag the lines that *might* be titles using setext-style.
   "might" because if they are, for instance, in a code section,
   then they are not titles at all. *)
let tag_setext main_loop lexemes =
  assert_well_formed lexemes;
  let rec loop pl res = function
    | (Newline as e1)::(Equal|Equals _ as e2)::tl -> (* might be a H1. *)
      begin
        match
          fsplit_rev
            ~f:(function
                  | (Space|Spaces _|Equal|Equals _)::tl -> Continue
                  | [] -> Split([],[])
                  | _::_ as l -> Split([], l))
            tl
        with
        | Some(rleft, (([]|(Newline|Newlines _)::_) as right)) ->
          loop [] (rleft@(e2::e1::pl@tag__maybe_h1 main_loop::res)) right
        | Some(rleft, right) ->
          loop [] (rleft@(e2::e1::pl@res)) right
        | None ->
          loop [] (e2::e1::pl@res) []
      end
    | (Newline as e1)::(Minus|Minuss _ as e2)::tl -> (* might be a H2. *)
      begin
        match
          fsplit_rev
            ~f:(function
                  | (Space|Spaces _|Minus|Minuss _)::tl -> Continue
                  | [] -> Split([],[])
                  | _::_ as l -> Split([], l))
            tl
        with
        | Some(rleft, (([]|(Newline|Newlines _)::_) as right)) ->
          loop [] (rleft@(e2::e1::pl@tag__maybe_h2 main_loop::res)) right
        | Some(rleft, right) ->
          loop [] (rleft@(e2::e1::pl@res)) right
        | None ->
          loop [] (e2::e1::pl@res) []
      end
    | (Newline | Newlines _ as e1)::tl ->
      loop [] (e1::pl@res) tl
    | e::tl ->
      loop (e::pl) res tl
    | [] ->
      pl@res
  in
  List.rev (loop [] [] lexemes)


let hr_m l =
  assert_well_formed l;
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

let hr_s l =
  assert_well_formed l;
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


exception NL_exception
exception Premature_ending

(* !!DO NOT DELETE THIS!!
The program that generates the generated part that follows right after.
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
  assert_well_formed l;
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
    | Spaces n :: tl -> assert(n>0); ((List.rev accu), ((Spaces(n - 1)) :: tl))
    | ((Newline | Newlines _ as e)) :: tl ->
        if no_nl then raise NL_exception else loop (e :: accu) n tl
    | e :: tl -> loop (e :: accu) n tl
    | [] -> raise Premature_ending
  in loop [] 0 l

let read_until_newline l = (* this has been patched post-generation *)
  assert_well_formed l;
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
let read_title main_loop n r _previous lexemes =
  let title, rest =
    let rec loop accu = function
      | (Hash|Hashs _) :: ((Newline|Newlines _) :: _ as l)
      | (Hash|Hashs _) :: (Space|Spaces _) :: ((Newline|Newlines _)::_ as l)
      | ((Newline|Newlines _) :: _ as l)
      | ([] as l) ->
         main_loop [] [] (List.rev accu), l
      | [Hash|Hashs _] ->
         main_loop [] [] (List.rev accu), []
      | (Hash|Hashs _ as x) :: tl ->
        loop (Word(string_of_t x)::accu) tl
      | x::tl ->
        loop (x::accu) tl
    in
    loop [] lexemes
  in
  match n with
  | 1 -> H1 title :: r, [Newline], rest
  | 2 -> H2 title :: r, [Newline], rest
  | 3 -> H3 title :: r, [Newline], rest
  | 4 -> H4 title :: r, [Newline], rest
  | 5 -> H5 title :: r, [Newline], rest
  | 6 -> H6 title :: r, [Newline], rest
  | _ -> assert false



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

(* blockquotes *)
let emailstyle_quoting main_loop r _p lexemes =
  assert_well_formed lexemes;
  if debug then eprintf "CALL: Omd_parser.emailstyle_quoting\n%!";
  let rec loop block cl =
    function
    | Newline::Greaterthan::(Newline::_ as tl) ->
      loop (Newline::cl@block) [] tl
    | Newline::Greaterthan::Space::tl ->
      loop (Newline::cl@block) [] tl
    | Newline::Greaterthan::Spaces 0::tl ->
      loop (Newline::cl@block) [Space] tl
    | Newline::Greaterthan::Spaces n::tl ->
      assert(n>0);
      loop (Newline::cl@block) [Spaces(n-1)] tl
    | Newlines 0::Greaterthan::Space::tl ->
      loop (Newlines 0::cl@block) [] tl
    | Newlines 0::Greaterthan::Spaces 0::tl ->
      loop (Newlines 0::cl@block) [Space] tl
    | Newlines 0::Greaterthan::Spaces n::tl ->
      assert(n>0);
      loop (Newlines 0::cl@block) [Spaces(n-1)] tl
    | (Newlines _::_ as l) | ([] as l) -> fix(List.rev(cl@block)), l
    | e::tl -> loop block (e::cl) tl
  in
  match loop [] [] lexemes with
  | block, tl ->
    if debug then
      eprintf "##############################\n%s\n\
                  ##############################\n%!" (string_of_tl block);
    (Blockquote(main_loop [] [] block)::r), [Newline], tl

(* maybe a reference *)
let maybe_reference rc r p l =
  assert_well_formed l;
  (* this function is called when we know it's not a link although
     it started with a '[' *)
  (* So it could be a reference or a link definition. *)
  let rec maybe_ref l =
    let text, remains = read_until_cbracket l in
    (* check that there is no ill-placed open bracket *)
    if (try ignore(read_until_obracket text); true
        with Premature_ending -> false) then
      raise Premature_ending; (* <-- ill-placed open bracket *)
    let blank, remains = read_until_obracket remains in
    (* check that there are no unwanted characters between CB and OB. *)
    if eat (let flag = ref true in
            function (* allow only a space, multiple spaces, or a newline *)
            | Newline -> !flag && (flag := false; true)
            | (Space|Spaces _) -> !flag && (flag := false; true)
            | _ -> false) blank <> [] then
        raise Premature_ending (* <-- not a regular reference *)
    else
      match read_until_cbracket remains with
      | [], remains ->
        let fallback = extract_fallback remains (Obracket::l) in
        let id = string_of_tl text in (* implicit anchor *)
        Some(((Ref(rc, id, id, fallback))::r), [Cbracket], remains)
      | id, remains ->
        let fallback = extract_fallback remains (Obracket::l) in
        Some(((Ref(rc, string_of_tl id, string_of_tl text, fallback))::r),
             [Cbracket], remains)
  in
  let rec maybe_nonregular_ref l =
    let text, remains = read_until_cbracket l in
    (* check that there is no ill-placed open bracket *)
    if (try ignore(read_until_obracket text); true
        with Premature_ending -> false) then
      raise Premature_ending; (* <-- ill-placed open bracket *)
    let fallback = extract_fallback remains (Obracket::l) in
    let id = string_of_tl text in (* implicit anchor *)
    Some(((Ref(rc, id, id, fallback))::r), [Cbracket], remains)
  in
  let rec maybe_def l =
    match read_until_cbracket l with
    | _, [] -> raise Premature_ending
    | id, (Colon::(Space|Spaces _)::remains)
    | id, (Colon::remains) ->
        begin
          match
            fsplit
              ~f:(function
                    | (Space|Spaces _|Newline|Newlines _):: _ as l -> Split([], l)
                    | e::tl -> Continue
                    | [] -> Split([],[]))
              remains
          with
            | None | Some([], _) -> raise Premature_ending
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
                let url =
                  let url = string_of_tl url in
                    if String.length url > 2 && url.[0] = '<'
                      && url.[String.length url - 1] = '>' then
                      String.sub url 1 (String.length url - 2)
                    else
                      url
                in
                  rc#add_ref (string_of_tl id) (string_of_tl title) url;
                  Some(r, [Newline], remains)
        end
    | _ -> raise Premature_ending
  in
    try
      maybe_ref l
    with | Premature_ending | NL_exception ->
      try
        maybe_def l
      with
      | Premature_ending | NL_exception ->
        try
          maybe_nonregular_ref l
        with
        | Premature_ending | NL_exception ->
          None


(** maybe a link *)
let maybe_link main_loop r p l =
  if debug then eprintf "# maybe_link\n";
  assert_well_formed l;
  let read_url name l =
    if debug then eprintf "# maybe_link>read_url\n";
    let url_and_maybetitle, tl = read_until_cparenth ~no_nl:false l in
    let url, title =
      try
        let url, title = read_until_dq ~no_nl:false url_and_maybetitle in
        if List.exists (function (Newline|Newlines _) -> true | _ -> false) url
        then raise Premature_ending;
        let title, blanks = read_until_dq ~no_nl:true title in
        if eat_blank blanks <> [] then raise Premature_ending;
        url, title
    with Premature_ending ->
      (* no title *)
      url_and_maybetitle, []
    in
    let url = match List.rev url with
      | (Newline|Space|Spaces _)::tl -> List.rev tl
      | _ -> url in
    Some(Url(string_of_tl url,name,string_of_tl title)::r, [Cparenthesis],tl)
  in
  let read_name l =
    if debug then eprintf "# maybe_link>read_name\n";
    try
      match read_until_cbracket l with
      | name, (Oparenthesis::tl) ->
        read_url (main_loop [] [Obracket] name) (eat_blank tl)
      | _ ->
        None
    with Premature_ending | NL_exception -> None
  in
  read_name l



(** code that starts with one or several backquote(s) *)
let bcode default_lang r p l =
  assert_well_formed l;
  let e, tl =
    match l with
    | (Backquote|Backquotes _ as e)::tl -> e, tl
    | _ -> (* bcode is wrongly called *) assert false
  in
  let rec code_block accu = function
    | [] ->
      None
    | Backquote::tl ->
      if e = Backquote then
        Some(List.rev accu, tl)
      else
        code_block (Backquote::accu) tl
    | (Backquotes n as b)::tl ->
      if e = b then
        Some(List.rev accu, tl)
      else
        code_block (b::accu) tl
    | Tag _::tl ->
      code_block accu tl
    | e::tl ->
      code_block (e::accu) tl
  in
  match code_block [] tl with
  | None -> None
  | Some(cb, l) ->
    if List.exists (function (Newline|Newlines _) -> true | _ -> false) cb
    then
      match cb with
      | Word lang :: (Space|Spaces _) :: Newline :: tl
      | Word lang :: Newline :: tl ->
         Some(Code_block(lang, string_of_tl tl) :: r, [Backquote], l)
      | Word lang :: (Space|Spaces _) :: Newlines 0 :: tl
      | Word lang :: Newlines 0 :: tl ->
         Some(Code_block(lang, string_of_tl(Newline::tl)) :: r, [Backquote], l)
      | Word lang :: (Space|Spaces _) :: Newlines n :: tl
      | Word lang :: Newlines n :: tl ->
        Some(Code_block(lang, string_of_tl(Newlines(n-1)::tl)) :: r,
             [Backquote], l)
      | Newline :: tl ->
        Some(Code_block(default_lang, string_of_tl tl) :: r, [Backquote], l)
      | _ ->
        Some(Code_block(default_lang, string_of_tl cb) :: r, [Backquote], l)
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
      Some(Code(default_lang, clean_bcode(string_of_tl cb)) :: r,
           [Backquote], l)

let icode default_lang r p l =
  assert_well_formed l;
  (* indented code:
     returns (r,p,l) where r is the result, p is the last thing read,
     l is the remains *)
  let dummy_tag = Tag(fun r p l -> None) in
  let accu = Buffer.create 42 in
  let rec loop = function
    | (Newline|Newlines _ as p), ((Space|Spaces(0|1))::_ as tl) ->
       (* 1, 2 or 3 spaces. *)
       (* -> Return what's been found as code because what follows isn't. *)
       Code_block(default_lang, Buffer.contents accu) :: r, [p], tl
    | (Newline|Newlines _ as p), Spaces(n)::tl ->
      assert(n>0);
      (* At least 4 spaces, it's still code. *)
      Buffer.add_string accu (string_of_t p);
      loop ((if n >= 4 then Spaces(n-4) else if n = 3 then Space else dummy_tag), tl)
    | (Newline|Newlines _ as p), (not_spaces::_ as tl) -> (* stop *)
      Code_block(default_lang, Buffer.contents accu) :: r, [p], tl
        (* -> Return what's been found as code because it's no more code. *)
    | p, e::tl ->
      Buffer.add_string accu (string_of_t p);
      (* html entities are to be converted later! *)
      loop (e, tl)
    | p, [] ->
      Buffer.add_string accu (string_of_t p);
      Code_block(default_lang, Buffer.contents accu)::r, [p], []
  in
    match l with
      | Spaces n::tl ->
        loop ((if n >= 4 then Spaces(n-4)
               else if n = 3 then Space
               else dummy_tag),
              tl)
      | _ -> assert false


let parse_list main_loop r p l =
  assert_well_formed l;
  if debug then begin
    eprintf "parse_list r=(%s) p=(%s) l=(%s)\n%!"
      "" (* (Omd_backend.sexpr_of_md (List.rev r)) *)
      "" (* (destring_of_tl p) *)
      (destring_of_tl ~limit:40 l);
  end;
  let module UO = struct type ordered = O | U end in
  let open UO in
  if debug then eprintf "parse_list: l=(%s)\n%!" (destring_of_tl l);
  let end_of_item (indent:int) l : tok split_action  = match l with
    | [] ->
       Split([],[])
    | Newlines 0 :: ((Spaces n) :: Greaterthan :: (Space | Spaces _) :: tl as s) ->
      assert(n>=0);
      if n+2 = indent+4 then (* blockquote *)
        match unindent (n+2) (Newline::s) with
          | Newline::block, rest ->
              Continue_with(List.rev(Newlines(1)::block), rest)
          | Newlines n::block, rest ->
              Continue_with(List.rev(Newlines(n+2)::block), rest)
          | block, rest ->
              Continue_with(Newlines 0::block, rest)
      else if n+2 >= indent+8 then (* code inside item *)
        match unindent (indent+4) (Newline::s) with
          | Newline::block, rest ->
              Continue_with(List.rev(Newlines(1)::block), rest)
          | Newlines n::block, rest ->
              Continue_with(List.rev(Newlines(n+2)::block), rest)
          | block, rest ->
              Continue_with(Newlines 0::block, rest)
      else
        Split([], l)
    | Newlines 0 :: (Spaces n :: tl as s) ->
      assert(n>=0);
      if n+2 >= indent+8 then (* code inside item *)
        match unindent (indent+4) (Newline::s) with
          | Newline::block, rest ->
              Continue_with(List.rev(Newlines(0)::block), rest)
          | Newlines n::block, rest ->
              Continue_with(List.rev(Newlines(n+1)::block), rest)
          | block, rest ->
              Continue_with(Newline::block, rest)
      else if n+2 >= indent+4 then (* new paragraph inside item *)
        match unindent (indent+4) (Newline::s) with
          | Newline::block, rest ->
              Continue_with(List.rev(Newlines(1)::block), rest)
          | Newlines n::block, rest ->
              Continue_with(List.rev(Newlines(n+2)::block), rest)
          | block, rest ->
              Continue_with(Newlines 0::block, rest)
      else
        Split([], l)
    | (Newlines _) :: _ -> (* n > 0 *)
       (* End of item, stop *)
       Split([], l)
    | Newline ::
        (
          ((Space|Spaces _) :: (Star|Minus|Plus) :: (Space|Spaces _):: _)
            | ((Space|Spaces _) :: Number _ :: Dot :: (Space|Spaces _) :: _)
            | ((Star|Minus|Plus) :: (Space|Spaces _):: _)
            | (Number _ :: Dot :: (Space|Spaces _) :: _)
                as tl) ->
       Split([Newline], tl)
    | Newline :: (Spaces _ as s) :: tl ->
      Continue_with
        ([s;
          Tag(fun r p ->
              function Spaces _::tl -> Some(r,p,Space::tl)
                     | _ -> None);
           Newline],
         tl)
    | _::_ ->
       Continue
  in
  let to_t l =
    assert_well_formed l;
    main_loop [] [Newline] l
  in
  let add (sublist:element) items =
    if debug then eprintf "add\n%!";
    match items with
    | [] -> assert false
    | (O,indents,item)::tl ->
      (U,indents,(item@[sublist]))::tl
    | (U,indents,item)::tl ->
      (U,indents,(item@[sublist]))::tl
  in
  let make_up ~p items : Omd_representation.element =
    if debug then eprintf "make_up p=%b\n%!" p;
    match items with
    | (U,_,item)::_ ->
      if p then
        Ulp((List.rev_map(fun (_,_,i) -> i) items))
      else
        Ul((List.rev_map(fun (_,_,i) -> i) items))
    | (O,_,item)::_ ->
      if p then
        Olp((List.rev_map(fun (_,_,i) -> i) items))
      else
        Ol((List.rev_map(fun (_,_,i) -> i) items))
    | [] ->
      failwith "make_up called with []" (* assert false *)
  in
  let rec list_items ~p indents items l =
    if debug then eprintf "list_items: p=%b l=(%s)\n%!" p (destring_of_tl l);
    match l with
    (* no more list items *)
    | [] ->
      make_up p items, l
    (* more list items *)
    (* new unordered items *)
    | (Star|Minus|Plus)::(Space|Spaces _)::tl ->
       begin
         match fsplit ~f:(end_of_item 0) tl with
         | None ->
           make_up p items, l
         | Some(new_item, rest) ->
           let p =
             p ||
             List.exists (function Newlines _ -> true | _ -> false) new_item
           in
           if debug then eprintf "new_item=%S\n%!" (destring_of_tl new_item);
           match indents with
           | [] ->
             assert(items = []);
             list_items ~p:p [0] ((U,[0],to_t new_item)::items) rest
           | 0::_ ->
             list_items ~p:p indents ((U,indents,to_t new_item)::items) rest
           | _::_ ->
             make_up p items, l
       end
    | Space::(Star|Minus|Plus)::(Space|Spaces _)::tl ->
       begin
         match fsplit ~f:(end_of_item 1) tl with
         | None -> make_up p items, l
         | Some(new_item, rest) ->
           match indents with
           | [] ->
             assert(items = []);
             list_items ~p:p [1] ((U,[1],to_t new_item)::items) rest
           | 1::_ ->
             list_items ~p:p indents ((U,indents,to_t new_item)::items) rest
           | i::_ ->
             if i > 1 then
               make_up p items, l
             else (* i < 1 : new sub list*)
               let sublist, remains =
                 list_items ~p:p (1::indents) [(U,1::indents,to_t new_item)] rest
               in
               list_items ~p:p indents (add sublist items) remains
       end
    | Spaces n::(Star|Minus|Plus)::(Space|Spaces _)::tl ->
       begin
         match fsplit ~f:(end_of_item (n+2)) tl with
         | None ->
           make_up p items, l
         | Some(new_item, rest) ->
           match indents with
           | [] ->
             if debug then eprintf "spaces[] l=(%S)\n%!" (string_of_tl l);
             assert(items = []); (* aïe... listes mal formées ?! *)
             list_items ~p:p [n+2] ((U,[n+2],to_t new_item)::items) rest
           | i::_ ->
             if debug then eprintf "spaces(%d::_) n=%d l=(%S)\n%!" i n (string_of_tl l);
             if i = n + 2 then
               list_items ~p:p indents ((U,indents,to_t new_item)::items) rest
             else if i < n + 2 then
               let sublist, remains =
                 list_items ~p:p
                   ((n+2)::indents)
                   [(U,(n+2)::indents,to_t new_item)]
                   rest
               in
               list_items ~p:p indents (add sublist items) remains
             else (* i > n + 2 *)
               make_up p items, l
       end
    (* new ordered items *)
    | Number _::Dot::(Space|Spaces _)::tl ->
       begin
         match fsplit ~f:(end_of_item 0) tl with
         | None ->
           make_up p items, l
         | Some(new_item, rest) ->
           assert_well_formed new_item;
           match indents with
           | [] ->
             assert(items = []);
             list_items ~p:p [0] ((O,[0],to_t new_item)::items) rest
           | 0::_ ->
             list_items ~p:p indents ((O,indents,to_t new_item)::items) rest
           | _::_ ->
             make_up p items, l
       end
    | Space::Number _::Dot::(Space|Spaces _)::tl ->
       begin
         match fsplit ~f:(end_of_item 1) tl with
         | None -> make_up p items, l
         | Some(new_item, rest) ->
           match indents with
           | [] ->
             assert(items = []);
             list_items ~p:p [1] ((O,[1],to_t new_item)::items) rest
           | 1::_ ->
             list_items ~p:p indents ((O,indents,to_t new_item)::items) rest
           | i::_ ->
             if i > 1 then
               make_up p items, l
             else (* i < 1 : new sub list*)
               let sublist, remains =
                 list_items ~p:p (1::indents) [(O,1::indents,to_t new_item)] rest
               in
               list_items ~p:p indents (add sublist items) remains
       end
    | Spaces n::Number _::Dot::(Space|Spaces _)::tl ->
       begin
         match fsplit ~f:(end_of_item (n+2)) tl with
         | None ->
           make_up p items, l
         | Some(new_item, rest) ->
           match indents with
           | [] ->
             if debug then eprintf "spaces[] l=(%S)\n%!" (string_of_tl l);
             assert(items = []); (* aïe... listes mal formées ?! *)
             list_items ~p:p [n+2] ((O,[n+2],to_t new_item)::items) rest
           | i::_ ->
             if debug then eprintf "spaces(%d::_) n=%d l=(%S)\n%!" i n (string_of_tl l);
             if i = n + 2 then
               list_items ~p:p indents ((O,indents,to_t new_item)::items) rest
             else if i < n + 2 then
               let sublist, remains =
                 list_items ~p:p
                   ((n+2)::indents)
                   [(O,(n+2)::indents,to_t new_item)]
                   rest
               in
               list_items ~p:p indents (add sublist items) remains
             else (* i > n + 2 *)
               make_up p items, l
       end
    (* *)
    | Newlines 0::((Star|Minus|Plus)::(Space|Spaces _)::_ as l)
    | Newlines 0::(Number _::Dot::(Space|Spaces _)::_ as l)
    | Newlines 0::((Space|Spaces _)::Star::(Space|Spaces _)::_ as l)
    | Newlines 0::((Space|Spaces _)::Number _::Dot::(Space|Spaces _)::_ as l)
      ->
        list_items ~p:true indents items l
    | _ ->
      if debug then
        begin
          let rec string_of_items items =
            match items with
            | [] -> ""
            | (O,indent::_,item)::tl -> sprintf "(O,%d,%s)" (indent) (Omd_backend.html_of_md item) ^ string_of_items tl
            | (U,indent::_,item)::tl -> sprintf "(U,%d,%s)" (indent) (Omd_backend.html_of_md item) ^ string_of_items tl
            | _ -> "(weird)"
          in
          eprintf "NALI parse_list: l=(%S) items=%s\n%!" (string_of_tl l) (string_of_items items)
        end;
    (* not a list item *)
      make_up p items, l
  in
  match list_items ~p:false [] [] l with
  | rp, l ->
    rp::r, [Newline], l


let spaces main_loop default_lang n r previous l =
  assert_well_formed l;
  assert (n > 0);
  match n, previous, l with
  | (1|2|3), ([] | [Newline|Newlines _]),
    (Star|Minus|Plus)::(Space|Spaces _)::tl ->
     (* unordered list *)
     parse_list main_loop r [] (make_space n::l)
  | (1|2|3), ([] | [Newline|Newlines _]),
    (Number _)::Dot::(Space|Spaces _)::tl ->
     (* ordered list *)
     parse_list main_loop r [] (make_space n::l)
  | (1|2|3), ([] | [Newlines _]), _::_ ->
     Text (" ")::r, previous, l
  | (1|2|3), ([] | [Newlines _]), [] ->
     r, previous, []
  | _, ([] | [Newlines _]), _ -> (* n>=4, indented code *)
     icode default_lang r previous (make_space n :: l)
  | 1, _, _ ->
     (Text " "::r), [Space], l
  | n, _, Newline :: tl ->
     (* 2 or more spaces before a newline, eat the newline *)
     Br::r, [Spaces(n-2)], tl
  | n, _, Newlines k :: tl ->
     (* 2 or more spaces before a newline, eat 1 newline *)
     Br::r, [Spaces(n-2)], (if k = 0 then Newline else Newlines(k-1)) :: tl
  | n, _, _ ->
     assert (n>1);
     (Text (String.make n ' ')::r), [Spaces(n-2)], l



let maybe_autoemail r p l =
  assert_well_formed l;
  match l with
  | Lessthan::tl ->
    begin
      match
        fsplit ~excl:(function (Newline|Newlines _|Space|Spaces _) :: _-> true
                             | [] -> true
                             | _ -> false)
          ~f:(function At::tl -> Split([],tl) | _ -> Continue)
          tl
      with
      | None -> None
      | Some(left, right) ->
        match
          fsplit ~excl:(function (Newline|Newlines _|Space|Spaces _) :: _-> true
                               | [] -> true
                               | _ -> false)
          ~f:(function Greaterthan::tl -> Split([],tl)
                     | Greaterthans 0::tl -> Split([],Greaterthan::tl)
                     | Greaterthans n::tl -> Split([],Greaterthans(n-1)::tl)
                     | _ -> Continue)
          right
        with
        | None -> None
        | Some(domain, tl) ->
          let email = string_of_tl left ^ "@" ^ string_of_tl domain in
          Some(Url("mailto:"^email,[Text email],"")::r,[Greaterthan],tl)
    end
  | _ -> failwith "Omd_parser.maybe_autoemail: wrong use of the function."

let is_hex s =
  String.length s > 1
  && (s.[0] = 'X' || s.[0] = 'x')
  && (let rec loop i =
        i = String.length s
        ||
        (match s.[i] with
         | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' ->
           loop (succ i)
         | _ -> false)
      in loop 1)

let main_parse extensions default_lang lexemes =
  assert_well_formed lexemes;
  let rc = new Omd_representation.ref_container in

  let rec main_loop_rev (r:t) (previous:Omd_representation.tok list)
      (lexemes:Omd_representation.tok list) =
    assert_well_formed lexemes;
    if debug then
      eprintf "main_loop_rev r=%s p=(%s) l=(%s)\n%!"
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
        main_loop_rev r p l
      | None ->
        main_loop_rev r previous tl
      end

    (* HTML comments *)
    | _, (Lessthan as t)::(Exclamation::Minuss 0::c as tl) ->
      begin
        let f = function
          | (Minuss _ as m)::(Greaterthan|Greaterthans _ as g)::tl ->
            Split([g;m], tl)
          | _ ->
            Continue
        in
        match fsplit ~f:f lexemes with
        | None ->
          begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
          | Some(r, p, l) -> main_loop_rev r p l
          end
        | Some (comments, new_tl) ->
          main_loop_rev
            (Html_comment(string_of_tl comments)::r)
            [Greaterthan]
            new_tl
      end

    (* email-style quoting / blockquote *)
    | ([]|[Newline|Newlines _]), Greaterthan::(Space|Spaces _)::_ ->
      begin
        let r, p, l =
          emailstyle_quoting main_loop r previous (Newline::lexemes)
        in main_loop_rev r p l
      end

    (* email-style quoting, with lines starting with spaces! *)
    | ([]|[Newline|Newlines _]), (Space|Spaces(0|1) as s)
      :: Greaterthan :: (Space|Spaces _)::_ ->
      (* It's 1, 2 or 3 spaces, not more because it wouldn't mean
         quoting anymore but code. *)
      begin
        let new_r, p, rest =
          let foo, rest =
            match unindent (fst(size s)) (Newline::lexemes) with
              | (Newline|Newlines _)::foo, rest -> foo, rest
              | res -> res
          in
          match
            emailstyle_quoting main_loop [] previous (Newline::(foo))
          with
          | new_r, p, [] -> new_r, p, rest
          | _ -> assert false
        in
        main_loop_rev (new_r@r) [Newline] rest
      end

    (* minus *)
    | ([]|[Newline|Newlines _]), (Minus|Minuss _ as t)::((Space|Spaces _) ::_ as tl) ->
      (* maybe hr *)
      begin match hr_m lexemes with
      | None -> (* no hr, so it could be a list *)
          begin match t with
          | Minus -> (* it's a list *)
            let md, new_p, new_l =
              parse_list main_loop r [] lexemes
            in
            main_loop_rev md new_p new_l
          | _ -> (* not a list *)
            begin match maybe_extension extensions r previous lexemes with
            | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
            | Some(r, p, l) -> main_loop_rev r p l
            end
          end
      | Some l -> (* hr *)
        main_loop_rev (Hr::r) [Newline] l
      end
    | ([]|[Newline|Newlines _]), (Minus|Minuss _ as t)::tl ->
      begin match hr_m lexemes with
      | None -> (* no hr, and it's not a list either
                   because it's not followed by spaces *)
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      | Some l -> (* hr *)
        main_loop_rev (Hr::r) [Newline] l
      end

    (* hashes *)
    | ([]|[(Newline|Newlines _)]), Hashs n :: (Space|Spaces _) :: tl
    | ([]|[(Newline|Newlines _)]), Hashs n :: tl -> (* hash titles *)
      let r, p, l = read_title main_loop (n+2) r previous tl in
      main_loop_rev r p l
    | ([]|[(Newline|Newlines _)]), Hash :: (Space|Spaces _) :: tl
    | ([]|[(Newline|Newlines _)]), Hash :: tl -> (* hash titles *)
      let r, p, l = read_title main_loop 1 r previous tl in
      main_loop_rev r p l
    | _, (Hash|Hashs _ as t) :: tl -> (* hash -- no title *)
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
      | Some(r, p, l) -> main_loop_rev r p l
      end

    (* At least 4 spaces, so it can only be code. *)
    | ([]|[Newline|Newlines _]), (Spaces n)::tl when n>=2 ->
      let r, p, l = icode default_lang r [Newline] lexemes in
      main_loop_rev r p l

    (* spaces after a newline: could lead to hr *)
    | ([]|[Newline|Newlines _]), ((Space|Spaces(0|1)) as xxxt) :: tl ->
      begin match hr_s tl with
      | None ->
        begin match hr_m tl with
        | None ->
          let r, p, l =
            spaces main_loop default_lang (fst(size xxxt)) r previous tl
          in
          main_loop_rev r p l
        | Some l ->
          main_loop_rev (Hr::r) [Newline] l
        end
      | Some l ->
        main_loop_rev (Hr::r) [Newline] l
      end

    (* spaces anywhere *)
    | _, ((Space|Spaces _) as t) :: tl ->
      (* too many cases to be handled here *)
      let r, p, l =
        spaces main_loop default_lang (fst(size t)) r previous tl
      in
      main_loop_rev r p l

    (* underscores *)
    | _, (Underscore as t) :: tl -> (* one "orphan" underscore, or emph *)
      (match uemph_or_bold 1 tl with
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      | Some(x, new_tl) ->
        main_loop_rev (Emph(main_loop [] [t] x) :: r) [t] new_tl
      )
    | _, (Underscores((0|1) as n) as t) :: tl ->
      (* 2 or 3 "orphan" underscores, or emph/bold *)
      (match uemph_or_bold (n+2) tl with
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      | Some(x, new_tl) ->
        if n = 0 then (* 1 underscore *)
          main_loop_rev (Bold(main_loop [] [t] x) :: r) [t] new_tl
        else (* 2 underscores *)
          main_loop_rev (Emph([Bold(main_loop [] [t] x)]) :: r) [t] new_tl
      )

    (* enumerated lists *)
    | ([]|[Newline|Newlines _]), (Number _) :: Dot :: (Space|Spaces _) :: tl ->
      let md, new_p, new_l =
        parse_list main_loop r [] lexemes
      in
      main_loop_rev md new_p new_l

    (* plus *)
    | ([]|[(Newline|Newlines _)]), Plus :: (Space|Spaces _) :: _ ->
      let md, new_p, new_l =
        parse_list main_loop r [] lexemes
      in
      main_loop_rev md new_p new_l

    (* stars *)
    | ([]|[(Newline|Newlines _)]), Star :: (Space|Spaces _) :: _ ->
      (* maybe hr or new list *)
      begin match hr_s lexemes with
      | Some l ->
        main_loop_rev (Hr::r) [Newline] l
      | None ->
        let md, new_p, new_l =
          parse_list main_loop r [] lexemes
        in
        main_loop_rev md new_p new_l
      end
    | ([]|[(Newline|Newlines _)]), Stars _ :: _ when hr_s lexemes <> None ->
      (* hr *)
      (match hr_s lexemes with
      | Some l -> main_loop_rev (Hr::r) [Newline] l
      | None -> assert false
      )
    | ([]|[(Newline|Newlines _)]), (Star as t) :: tl -> (* maybe hr *)
      begin match hr_s lexemes with
      | Some l ->
        main_loop_rev (Hr::r) [Newline] l
      | None ->
        (match semph_or_bold 1 tl with
        | Some(x, new_tl) ->
          main_loop_rev (Emph(main_loop [] [t] x) :: r) [t] new_tl
        | None ->
          begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
          | Some(r, p, l) -> main_loop_rev r p l
          end
        )
      end
    | _, (Star as t) :: tl -> (* one "orphan" star, or emph // can't be hr *)
      (match semph_or_bold 1 tl with
      | Some(x, new_tl) ->
        main_loop_rev (Emph(main_loop [] [t] x) :: r) [t] new_tl
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      )
    | _, (Stars((0|1) as n) as t) :: tl ->
      (* 2 or 3 "orphan" stars, or emph/bold *)
      (match semph_or_bold (n+2) tl with
      | Some(x, new_tl) ->
        if n = 0 then
          main_loop_rev (Bold(main_loop [] [t] x) :: r) [t] new_tl
        else
          main_loop_rev (Emph([Bold(main_loop [] [t] x)]) :: r) [t] new_tl
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      )

    (* backslashes *)
    | _, Backslash :: (Newline as t) :: tl -> (* \\n *)
      main_loop_rev (Br :: r) [t] tl
    | _, Backslash :: Newlines 0 :: tl -> (* \\n\n\n\n... *)
      main_loop_rev (Br :: r) [Backslash; Newline] (Newline :: tl)
    | _, Backslash :: Newlines n :: tl -> assert (n >= 0); (* \\n\n\n\n... *)
      main_loop_rev (Br :: r) [Backslash; Newline]
        (Newlines (n-1) :: tl)
    | _, Backslash :: (Backquote as t) :: tl -> (* \` *)
      main_loop_rev (Text ("`") :: r) [t] tl
    | _, Backslash :: Backquotes 0 :: tl -> (* \````... *)
      main_loop_rev (Text ("`") :: r) [Backslash; Backquote] (Backquote :: tl)
    | _, Backslash :: Backquotes n :: tl -> assert (n >= 0); (* \````... *)
      main_loop_rev (Text ("`") :: r) [Backslash; Backquote]
        (Backquotes (n-1) :: tl)
    | _, Backslash :: (Star as t) :: tl -> (* \* *)
      main_loop_rev (Text ("*") :: r) [t] tl
    | _, Backslash :: Stars 0 :: tl -> (* \****... *)
      main_loop_rev (Text ("*") :: r) [Backslash; Star] (Star :: tl)
    | _, Backslash :: Stars n :: tl -> assert (n >= 0); (* \****... *)
      main_loop_rev (Text ("*") :: r) [Backslash; Star] (Stars (n-1) :: tl)
    | _, Backslash :: (Underscore as t) :: tl -> (* \_ *)
      main_loop_rev (Text ("_") :: r) [t] tl
    | _, Backslash :: Underscores 0 :: tl -> (* \___... *)
      main_loop_rev (Text ("_") :: r) [Backslash; Underscore] (Underscore :: tl)
    | _, Backslash :: Underscores n :: tl -> assert (n >= 0); (* \___... *)
      main_loop_rev (Text ("_") :: r) [Backslash; Underscore]
        (Underscores (n-1) :: tl)
    | _, Backslash :: (Obrace as t) :: tl -> (* \{ *)
      main_loop_rev (Text ("{") :: r) [t] tl
    | _, Backslash :: Obraces 0 :: tl -> (* \{{{... *)
      main_loop_rev (Text ("{") :: r) [Backslash; Obrace] (Obrace :: tl)
    | _, Backslash :: Obraces n :: tl -> assert (n >= 0); (* \{{{... *)
      main_loop_rev (Text ("{") :: r) [Backslash; Obrace] (Obraces (n-1) :: tl)
    | _, Backslash :: (Cbrace as t) :: tl -> (* \} *)
      main_loop_rev (Text ("}") :: r) [t] tl
    | _, Backslash :: Cbraces 0 :: tl -> (* \}}}... *)
      main_loop_rev (Text ("}") :: r) [Backslash; Cbrace] (Cbrace :: tl)
    | _, Backslash :: Cbraces n :: tl -> assert (n >= 0); (* \}}}... *)
      main_loop_rev (Text ("}") :: r) [Backslash; Cbrace] (Cbraces (n-1) :: tl)
    | _, Backslash :: (Obracket as t) :: tl -> (* \[ *)
      main_loop_rev (Text ("[") :: r) [t] tl
    | _, Backslash :: Obrackets 0 :: tl -> (* \[[[... *)
      main_loop_rev (Text ("[") :: r) [Backslash; Obracket] (Obracket :: tl)
    | _, Backslash :: Obrackets n :: tl -> assert (n >= 0); (* \[[[... *)
      main_loop_rev (Text ("[") :: r) [Backslash; Obracket] (Obrackets (n-1) :: tl)
    | _, Backslash :: (Cbracket as t) :: tl -> (* \} *)
      main_loop_rev (Text ("]") :: r) [t] tl
    | _, Backslash :: Cbrackets 0 :: tl -> (* \}}}... *)
      main_loop_rev (Text ("]") :: r) [Backslash; Cbracket] (Cbracket :: tl)
    | _, Backslash :: Cbrackets n :: tl -> assert (n >= 0); (* \}}}... *)
      main_loop_rev (Text ("]") :: r) [Backslash; Cbracket] (Cbrackets (n-1) :: tl)
    | _, Backslash :: (Oparenthesis as t) :: tl -> (* \( *)
      main_loop_rev (Text ("(") :: r) [t] tl
    | _, Backslash :: Oparenthesiss 0 :: tl -> (* \(((... *)
      main_loop_rev (Text ("(") :: r) [Backslash; Oparenthesis] (Oparenthesis :: tl)
    | _, Backslash :: Oparenthesiss n :: tl -> assert (n >= 0); (* \(((... *)
      main_loop_rev (Text ("(") :: r) [Backslash; Oparenthesis]
        (Oparenthesiss (n-1) :: tl)
    | _, Backslash :: (Cparenthesis as t) :: tl -> (* \) *)
      main_loop_rev (Text (")") :: r) [t] tl
    | _, Backslash :: Cparenthesiss 0 :: tl -> (* \)))... *)
      main_loop_rev (Text (")") :: r) [Backslash; Cparenthesis]
        (Cparenthesis :: tl)
    | _, Backslash :: Cparenthesiss n :: tl -> assert (n >= 0); (* \)))... *)
      main_loop_rev (Text (")") :: r) [Backslash; Cparenthesis]
        (Cparenthesiss (n-1) :: tl)
    | _, Backslash :: (Plus as t) :: tl -> (* \+ *)
      main_loop_rev (Text ("+") :: r) [t] tl
    | _, Backslash :: Pluss 0 :: tl -> (* \+++... *)
      main_loop_rev (Text ("+") :: r) [Backslash; Plus] (Plus :: tl)
    | _, Backslash :: Pluss n :: tl -> assert (n >= 0); (* \+++... *)
      main_loop_rev (Text ("+") :: r) [Backslash; Plus] (Pluss (n-1) :: tl)
    | _, Backslash :: (Minus as t) :: tl -> (* \- *)
      main_loop_rev (Text ("-") :: r) [t] tl
    | _, Backslash :: Minuss 0 :: tl -> (* \---... *)
      main_loop_rev (Text ("-") :: r) [Backslash; Minus] (Minus :: tl)
    | _, Backslash :: Minuss n :: tl -> assert (n >= 0); (* \---... *)
      main_loop_rev (Text ("-") :: r) [Backslash; Minus] (Minuss (n-1) :: tl)
    | _, Backslash :: (Dot as t) :: tl -> (* \. *)
      main_loop_rev (Text (".") :: r) [t] tl
    | _, Backslash :: Dots 0 :: tl -> (* \....... *)
      main_loop_rev (Text (".") :: r) [Backslash; Dot] (Dot :: tl)
    | _, Backslash :: Dots n :: tl -> assert (n >= 0); (* \....... *)
      main_loop_rev (Text (".") :: r) [Backslash; Dot] (Dots (n-1) :: tl)
    | _, Backslash :: (Exclamation as t) :: tl -> (* \! *)
      main_loop_rev (Text ("!") :: r) [t] tl
    | _, Backslash :: Exclamations 0 :: tl -> (* \!!!... *)
      main_loop_rev (Text ("!") :: r) [Backslash; Exclamation] (Exclamation :: tl)
    | _, Backslash :: Exclamations n :: tl -> assert (n >= 0); (* \!!!... *)
      main_loop_rev (Text ("!") :: r) [Backslash; Exclamation]
        (Exclamations (n-1) :: tl)
    | _, Backslash :: (Hash as t) :: tl -> (* \# *)
      main_loop_rev (Text ("#") :: r) [t] tl
    | _, Backslash :: Hashs 0 :: tl -> (* \###... *)
      main_loop_rev (Text ("#") :: r) [Backslash; Hash] (Hash :: tl)
    | _, Backslash :: Hashs n :: tl -> assert (n >= 0); (* \###... *)
      main_loop_rev (Text ("#") :: r) [Backslash; Hash] (Hashs (n-1) :: tl)
    | _, (Backslashs 0 as t) :: tl -> (* \\\\... *)
      main_loop_rev (Text ("\\") :: r) [t] tl
    | _, (Backslashs n as t) :: tl -> (* \\\\... *)
      if n mod 2 = 0 then
        main_loop_rev (Text(String.make ((n-2)/2) '\\') :: r) [t] tl
      else
        main_loop_rev (Text(String.make ((n-2)/2) '\\') :: r) [t] (Backslash :: tl)
    | _, Backslash::[] ->
      main_loop_rev (Text "\\" :: r) [] []
    | _, Backslash::tl ->
      main_loop_rev (Text "\\" :: r) [Backslash] tl

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
        main_loop_rev (Url(url,[Text url],"")::r) [] new_tl
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      end


    (* Word(w) *)
    | _, Word w::tl ->
      main_loop_rev (Text w :: r) [Word w] tl

    (* newline at the end *)
    | _, [Newline] ->
      NL::r

    (* named html entity *)
    | _, Ampersand::((Word w::((Semicolon|Semicolons _) as s)::tl) as tl2) ->
      if StringSet.mem w htmlcodes_set then
        begin match s with
        | Semicolon ->
          main_loop_rev (Html("&"^w^";")::r) [s] tl
        | Semicolons 0 ->
          main_loop_rev (Html("&"^w^";")::r) [s] (Semicolon::tl)
        | Semicolons n ->
          main_loop_rev (Html("&"^w^";")::r) [s] (Semicolons(n-1)::tl)
        | _ -> assert false
        end
      else
        main_loop_rev (Html("&amp;")::r) [] tl2

    (* digit-coded html entity *)
    | _, Ampersand::((Hash::Number w::((Semicolon|Semicolons _) as s)::tl)
                        as tl2) ->
      if String.length w <= 4 then
        begin match s with
        | Semicolon ->
          main_loop_rev (Html("&#"^w^";")::r) [s] tl
        | Semicolons 0 ->
          main_loop_rev (Html("&#"^w^";")::r) [s] (Semicolon::tl)
        | Semicolons n ->
          main_loop_rev (Html("&#"^w^";")::r) [s] (Semicolons(n-1)::tl)
        | _ -> assert false
        end
      else
        main_loop_rev (Html("&amp;")::r) [] tl2

    (* maybe hex digit-coded html entity *)
    | _, Ampersand::((Hash::Word w::((Semicolon|Semicolons _) as s)::tl)
                        as tl2) when is_hex w ->
      if String.length w <= 4 then
        begin match s with
        | Semicolon ->
          main_loop_rev (Html("&#"^w^";")::r) [s] tl
        | Semicolons 0 ->
          main_loop_rev (Html("&#"^w^";")::r) [s] (Semicolon::tl)
        | Semicolons n ->
          main_loop_rev (Html("&#"^w^";")::r) [s] (Semicolons(n-1)::tl)
        | _ -> assert false
        end
      else
        main_loop_rev (Html("&amp;")::r) [] tl2


    (* Ampersand *)
    | _, Ampersand::tl ->
      main_loop_rev (Html("&amp;")::r) [Ampersand] tl

    (* 2 Ampersands *)
    | _, Ampersands(0)::tl ->
      main_loop_rev (Html("&amp;")::r) [] (Ampersand::tl)

    (* Several Ampersands (more than 2) *)
    | _, Ampersands(n)::tl ->
      main_loop_rev (Html("&amp;")::r) [] (Ampersands(n-1)::tl)

    (* backquotes *)
    | _, (Backquote|Backquotes _ as t)::tl ->
      begin match bcode default_lang r previous lexemes with
      | Some(r, p, l) -> main_loop_rev r p l
      | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      end

    (* HTML *)
    (* <br/> and <hr/> with or without space(s) *)
    | _, (Lessthan::Word("br"|"hr" as w)::Slash
          ::(Greaterthan|Greaterthans _ as g)::tl)
    | _, (Lessthan::Word("br"|"hr" as w)::(Space|Spaces _)::Slash
          ::(Greaterthan|Greaterthans _ as g)::tl) ->
      begin match g with
      | Greaterthans 0 ->
        main_loop_rev (Html("<"^w^" />")::r) [Greaterthan] (Greaterthan::tl)
      | Greaterthans n ->
        main_loop_rev (Html("<"^w^" />")::r) [Greaterthan] (Greaterthans(n-1)::tl)
      | _ ->
        main_loop_rev (Html("<"^w^" />")::r) [Greaterthan] tl
      end

    (* block html *)
    | ([]|[Newline|Newlines _]),
        ((Lessthan as t)::Word(tagname)
         ::((Space|Spaces _|Greaterthan|Greaterthans _) as x)
         ::tl) ->
      if StringSet.mem tagname inline_htmltags_set then
        main_loop_rev r [Word ""] lexemes
      else if not (!blind_html || StringSet.mem tagname htmltags_set) then
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      else
        let read_html() =
          let rec loop accu n = function
            | Lessthan::Slash::Word tn::Greaterthans 0::tl ->
              loop accu n
                (Lessthan::Slash::Word tn::Greaterthan::Greaterthan::tl)
            | Lessthan::Slash::Word tn::Greaterthans g::tl ->
              loop accu n
                (Lessthan::Slash::Word tn::Greaterthan::Greaterthans(g-1)::tl)
            | Lessthan::Slash::Word tn::Greaterthan::tl ->
              if tn = tagname then
                if n = 0 then
                  List.rev (Greaterthan::Word tn::Slash::Lessthan::accu), tl
                else
                  loop (Greaterthan::Word tn::Slash::Lessthan::accu)
                    (n-1) tl
              else
                loop (Greaterthan::Word tn::Slash::Lessthan::accu) n tl
            | Lessthan::Word(tn)::tl ->
              if tn = tagname then
                loop (Word tn::Lessthan::accu) (n+1) tl
              else
                loop (Word tn::Lessthan::accu) n tl
            | x::tl ->
              loop (x::accu) n tl
            | [] ->
              List.rev accu, []
          in
          loop [x;Word(tagname);Lessthan] 0 tl
        in
        let html, tl = read_html() in
        main_loop_rev (Html_block(string_of_tl html)::r) [Greaterthan] tl

    (* inline html *)
    | _, ((Lessthan as t)
          ::((Word(tagname) as w)
          ::((Space|Spaces _|Greaterthan|Greaterthans _)
          ::_ as html_stuff) as tl)) ->
      if (!strict_html && not(StringSet.mem tagname inline_htmltags_set))
        || not(!blind_html || StringSet.mem tagname htmltags_set)
      then
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      else
        let read_html() =
          let tag s = tag__md [Html s] in
          let rec loop accu n = function
            | Lessthan::Word("img"|"br"|"hr" as tn)::tl ->
              (* MAYBE self-closing tags *)
              begin
                let b, tl = read_until_gt tl in
                match List.rev b with
                | Slash::_ ->
                  loop
                    (tag(sprintf "<%s%s>" tn (string_of_tl b)) :: accu)
                    n
                    tl
                | _ ->
                  loop
                    (tag(sprintf "<%s%s>" tn (string_of_tl b)) :: accu)
                    (if tn = tagname then n+1 else n)
                    tl
              end
            | Lessthan::Slash::Word tn::Greaterthans 0::tl ->
              loop accu n
                (Lessthan::Slash::Word tn::Greaterthan::Greaterthan::tl)
            | Lessthan::Slash::Word tn::Greaterthans g::tl ->
              loop accu n
                (Lessthan::Slash::Word tn::Greaterthan::Greaterthans(g-1)::tl)
            | Lessthan::Slash::Word tn::Greaterthan::tl -> (* </word> ... *)
              if tn = tagname then
                if n = 0 then
                  List.rev (tag(sprintf "</%s>" tn)::accu), tl
                else
                  loop (tag(sprintf "</%s>" tn)::accu) (n-1) tl
              else
                loop (tag(sprintf "</%s>" tn)::accu) n tl
            | Lessthan::Word tn::tl -> (* <word... *)
              let b, tl = read_until_gt tl in
              loop
                (tag(sprintf "<%s%s>" tn (string_of_tl b)) :: accu)
                (if tn = tagname then n+1 else n)
                tl
            | x::tl ->
              loop (x::accu) n tl
            | [] ->
              List.rev accu, []
          in
          let b, tl = read_until_gt html_stuff in
          if (try ignore(read_until_lt b); false
            with Premature_ending -> true) then
            (* there must not be any '<' in b *)
            loop [tag(Printf.sprintf "<%s%s>" tagname
                        (string_of_tl b))] 0 tl
          else
            raise Premature_ending
        in
        (match try Some(read_html()) with Premature_ending -> None with
        | Some(html, tl) ->
          main_loop_rev (main_loop_rev [] [] html @ r) [Greaterthan] tl
        | None ->
          main_loop_rev (Text(string_of_t t^tagname)::r) [w] html_stuff
        )
    (* / end of inline HTML. *)

    (* < : emails *)
    | _, (Lessthan as t)::tl ->
      begin match maybe_autoemail r previous lexemes with
        | Some(r,p,l) -> main_loop_rev r p l
        | None ->
        begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
        end
      end

    (* line breaks *)
    | _, Newline::tl ->
      main_loop_rev (NL::r) [Newline] tl
    | _, Newlines _::tl ->
      main_loop_rev (NL::NL::r) [Newline] tl

    (* [ *)
    | _, (Obracket as t)::tl ->
      begin match maybe_link main_loop r previous tl with
      | Some(r, p, l) -> main_loop_rev r p l
      | None ->
        match maybe_reference rc r previous tl with
        | Some(r, p, l) -> main_loop_rev r p l
        | None ->
          begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
          | Some(r, p, l) -> main_loop_rev r p l
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
             main_loop_rev (Img("", string_of_tl url, string_of_tl title) :: r)
               [Cparenthesis] tl
           | None ->
             main_loop_rev (Img("", string_of_tl b, "") :: r)
               [Cparenthesis] tl
         end
       with
       | NL_exception ->
         begin match maybe_extension extensions r previous lexemes with
         | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
         | Some(r, p, l) -> main_loop_rev r p l
         end
      )

    (* img ref *)
    | _, (Exclamation as t)
      ::Obracket::Cbracket::Obracket::tl ->
      (* ref image insertion with no "alt" *)
      (* ![][ref] *)
      (try
         let id, tl = read_until_cbracket ~no_nl:true tl in
         let fallback = extract_fallback tl lexemes in
         let r = Img_ref(rc, string_of_tl id, "", fallback) :: r in
         main_loop_rev r [Cbracket] tl
       with NL_exception ->
         begin match maybe_extension extensions r previous lexemes with
         | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
         | Some(r, p, l) -> main_loop_rev r p l
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
              main_loop_rev r [Cparenthesis] rest
            with
            | NL_exception
            (* if NL_exception was raised, then fall back to "text" *)
            | Premature_ending ->
              begin match maybe_extension extensions r previous lexemes with
              | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
              | Some(r, p, l) -> main_loop_rev r p l
              end
           )
         | alt, Obracket::Word(id)::Cbracket::ntl
         | alt, Obracket::(Space|Spaces _)::Word(id)::Cbracket::ntl
         | alt, Obracket::(Space|Spaces _)::Word(id)::(Space|Spaces _)
           ::Cbracket::ntl
         | alt, Obracket::Word(id)::(Space|Spaces _)::Cbracket::ntl ->
           let fallback = extract_fallback ntl lexemes in
           main_loop_rev (Img_ref(rc, id, string_of_tl alt, fallback)::r) [Cbracket] ntl
         | alt, Obracket::((Newline|Space|Spaces _|Word _|Number _)::_
                              as ntl) ->
           (try
              match read_until_cbracket ~no_nl:false ntl with
              | [], rest -> raise Premature_ending
              | id, rest ->
                let fallback = extract_fallback rest lexemes in
                main_loop_rev
                  (Img_ref(rc, string_of_tl id, string_of_tl alt, fallback)::r)
                  [Cbracket]
                  rest
            with
            | Premature_ending
            | NL_exception ->
              begin match maybe_extension extensions r previous lexemes with
              | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
              | Some(r, p, l) -> main_loop_rev r p l
              end
           )
         | _ ->
           begin match maybe_extension extensions r previous lexemes with
           | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
           | Some(r, p, l) -> main_loop_rev r p l
           end
       with
       | Premature_ending ->
         begin match maybe_extension extensions r previous lexemes with
         | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
         | Some(r, p, l) -> main_loop_rev r p l
         end
      )

    | _,
      (At|Bar|Caret|Cbrace|Colon|Comma|Cparenthesis|Cbracket|Dollar
          |Dot|Doublequote|Exclamation|Equal|Minus|Obrace|Oparenthesis
          |Percent|Plus|Question|Quote|Semicolon|Slash|Tab|Tilde
          |Greaterthan as t)::tl
      ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, (Number _  as t):: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t t)::r) [t] tl
      | Some(r, p, l) -> main_loop_rev r p l
      end

    (* generated code: *)
    | _, Ats(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t At)::r) [At] (At::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Ats(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t At)::r) [At] (Ats(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Bars(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Bar)::r) [Bar] (Bar::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Bars(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Bar)::r) [Bar] (Bars(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Carets(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Caret)::r) [Caret] (Caret::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Carets(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Caret)::r) [Caret] (Carets(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Cbraces(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Cbrace)::r) [Cbrace] (Cbrace::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Cbraces(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Cbrace)::r) [Cbrace] (Cbraces(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Cbrackets(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Cbracket)::r) [Cbracket] (Cbracket::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Cbrackets(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Cbracket)::r) [Cbracket] (Cbrackets(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Colons(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Colon)::r) [Colon] (Colon::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Colons(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Colon)::r) [Colon] (Colons(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Commas(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Comma)::r) [Comma] (Comma::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Commas(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Comma)::r) [Comma] (Commas(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Cparenthesiss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Cparenthesis)::r) [Cparenthesis] (Cparenthesis::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Cparenthesiss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Cparenthesis)::r) [Cparenthesis] (Cparenthesiss(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Dollars(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Dollar)::r) [Dollar] (Dollar::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Dollars(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Dollar)::r) [Dollar] (Dollars(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Dots(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Dot)::r) [Dot] (Dot::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Dots(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Dot)::r) [Dot] (Dots(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Doublequotes(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Doublequote)::r) [Doublequote] (Doublequote::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Doublequotes(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Doublequote)::r) [Doublequote] (Doublequotes(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Equals(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Equal)::r) [Equal] (Equal::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Equals(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Equal)::r) [Equal] (Equals(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Exclamations(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Exclamation)::r) [Exclamation] (Exclamation::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Exclamations(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Exclamation)::r) [Exclamation] (Exclamations(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Greaterthans(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Greaterthan)::r) [Greaterthan] (Greaterthan::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Greaterthans(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Greaterthan)::r) [Greaterthan] (Greaterthans(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Lessthans(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Lessthan)::r) [Lessthan] (Lessthan::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Lessthans(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Lessthan)::r) [Lessthan] (Lessthans(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Minuss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Minus)::r) [Minus] (Minus::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Minuss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Minus)::r) [Minus] (Minuss(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Obraces(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Obrace)::r) [Obrace] (Obrace::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Obraces(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Obrace)::r) [Obrace] (Obraces(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Obrackets(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Obracket)::r) [Obracket] (Obracket::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Obrackets(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Obracket)::r) [Obracket] (Obrackets(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Oparenthesiss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Oparenthesis)::r) [Oparenthesis] (Oparenthesis::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Oparenthesiss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Oparenthesis)::r) [Oparenthesis] (Oparenthesiss(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Percents(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Percent)::r) [Percent] (Percent::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Percents(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Percent)::r) [Percent] (Percents(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Pluss(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Plus)::r) [Plus] (Plus::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Pluss(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Plus)::r) [Plus] (Pluss(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Questions(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Question)::r) [Question] (Question::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Questions(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Question)::r) [Question] (Questions(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Quotes(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Quote)::r) [Quote] (Quote::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Quotes(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Quote)::r) [Quote] (Quotes(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Semicolons(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Semicolon)::r) [Semicolon] (Semicolon::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Semicolons(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Semicolon)::r) [Semicolon] (Semicolons(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Slashs(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Slash)::r) [Slash] (Slash::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Slashs(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Slash)::r) [Slash] (Slashs(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Stars(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Star)::r) [Star] (Stars(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Tabs(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Tab)::r) [Tab] (Tab::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Tabs(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Tab)::r) [Tab] (Tabs(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Tildes(0) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Tilde)::r) [Tilde] (Tilde::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Tildes(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Tilde)::r) [Tilde] (Tildes(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, Underscores(n) :: tl ->
      begin match maybe_extension extensions r previous lexemes with
      | None -> main_loop_rev (Text(string_of_t Underscore)::r) [Underscore] (Underscores(n-1)::tl)
      | Some(r, p, l) -> main_loop_rev r p l
      end
  (* /generated code *)


  and main_loop (r:r) (previous:p) (lexemes:l) =
    assert_well_formed lexemes;
    List.rev (main_loop_rev r previous lexemes)

  in
  main_loop [] [] (tag_setext main_loop lexemes)


let parse ?(extensions=[]) ?(lang="") lexemes =
  main_parse extensions lang lexemes

(******************************************************************************)

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Printf
open Omd_representation
open Omd_utils

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

module type Env = sig
  val rc: Omd_representation.ref_container
  val extensions : Omd_representation.extensions
  val default_lang : string
  val gh_uemph_or_bold_style : bool
  val blind_html : bool
  val strict_html : bool
end

module Unit = struct end

module Default_env (Unit:sig end) : Env = struct
  let rc = new Omd_representation.ref_container
  let extensions = []
  let default_lang = ""
  let gh_uemph_or_bold_style = true
  let blind_html = false
  let strict_html = false
end

module Make (Env:Env) =
struct
  include Env

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

  (** N.B. it seems that there is no clear distinction between inline
      tags and block-level tags: in HTML4 it was not clear, in HTML5
      it's even more complicated. So, the choice *here* is to specify
      a set of tags considered as "inline", cf. [inline_htmltags_set].
      So there will be inline tags, non-inline tags, and unknown
      tags.*)

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
         | %s::%s::tl ->
            if trackfix then eprintf \"%s 1\\n%!\";
            loop accu (%ss 0::tl)
         | %ss n::%s::tl ->
            if trackfix then eprintf \"%s 2\\n%!\";
            loop accu (%ss(n+1)::tl)
         | %s::%ss n::tl ->
            if trackfix then eprintf \"%s 3\\n%!\";
            loop accu (%ss(n+1)::tl)
         | %ss a::%ss b::tl ->
            if trackfix then eprintf \"%s 4\\n%!\";
            loop accu (%ss(a+b+2)::tl)"
         e e e e e e e e e e e e e e e e)
         ["Ampersand"; "At"; "Backquote"; "Backslash"; "Bar"; "Caret"; "Cbrace"; "Colon"; "Comma"; "Cparenthesis"; "Cbracket"; "Dollar"; "Dot"; "Doublequote"; "Exclamation"; "Equal"; "Greaterthan"; "Hash"; "Lessthan"; "Minus"; "Newline"; "Obrace"; "Oparenthesis"; "Obracket"; "Percent"; "Plus"; "Question"; "Quote"; "Semicolon"; "Slash"; "Space"; "Star"; "Tab"; "Tilde"; "Underscore"];
         print_string "| x::tl -> loop (x::accu) tl\n| [] -> List.rev accu\n"; *)
      | Ampersand::Ampersand::tl ->
        if trackfix then eprintf "Ampersand 1\n";
        loop accu (Ampersands 0::tl)
      | Ampersands n::Ampersand::tl ->
        if trackfix then eprintf "Ampersand 2\n";
        loop accu (Ampersands(n+1)::tl)
      | Ampersand::Ampersands n::tl ->
        if trackfix then eprintf "Ampersand 3\n";
        loop accu (Ampersands(n+1)::tl)
      | Ampersands a::Ampersands b::tl ->
        if trackfix then eprintf "Ampersand 4\n";
        loop accu (Ampersands(a+b+2)::tl)
      | At::At::tl ->
        if trackfix then eprintf "At 1\n";
        loop accu (Ats 0::tl)
      | Ats n::At::tl ->
        if trackfix then eprintf "At 2\n";
        loop accu (Ats(n+1)::tl)
      | At::Ats n::tl ->
        if trackfix then eprintf "At 3\n";
        loop accu (Ats(n+1)::tl)
      | Ats a::Ats b::tl ->
        if trackfix then eprintf "At 4\n";
        loop accu (Ats(a+b+2)::tl)
      | Backquote::Backquote::tl ->
        if trackfix then eprintf "Backquote 1\n";
        loop accu (Backquotes 0::tl)
      | Backquotes n::Backquote::tl ->
        if trackfix then eprintf "Backquote 2\n";
        loop accu (Backquotes(n+1)::tl)
      | Backquote::Backquotes n::tl ->
        if trackfix then eprintf "Backquote 3\n";
        loop accu (Backquotes(n+1)::tl)
      | Backquotes a::Backquotes b::tl ->
        if trackfix then eprintf "Backquote 4\n";
        loop accu (Backquotes(a+b+2)::tl)
      | Backslash::Backslash::tl ->
        if trackfix then eprintf "Backslash 1\n";
        loop accu (Backslashs 0::tl)
      | Backslashs n::Backslash::tl ->
        if trackfix then eprintf "Backslash 2\n";
        loop accu (Backslashs(n+1)::tl)
      | Backslash::Backslashs n::tl ->
        if trackfix then eprintf "Backslash 3\n";
        loop accu (Backslashs(n+1)::tl)
      | Backslashs a::Backslashs b::tl ->
        if trackfix then eprintf "Backslash 4\n";
        loop accu (Backslashs(a+b+2)::tl)
      | Bar::Bar::tl ->
        if trackfix then eprintf "Bar 1\n";
        loop accu (Bars 0::tl)
      | Bars n::Bar::tl ->
        if trackfix then eprintf "Bar 2\n";
        loop accu (Bars(n+1)::tl)
      | Bar::Bars n::tl ->
        if trackfix then eprintf "Bar 3\n";
        loop accu (Bars(n+1)::tl)
      | Bars a::Bars b::tl ->
        if trackfix then eprintf "Bar 4\n";
        loop accu (Bars(a+b+2)::tl)
      | Caret::Caret::tl ->
        if trackfix then eprintf "Caret 1\n";
        loop accu (Carets 0::tl)
      | Carets n::Caret::tl ->
        if trackfix then eprintf "Caret 2\n";
        loop accu (Carets(n+1)::tl)
      | Caret::Carets n::tl ->
        if trackfix then eprintf "Caret 3\n";
        loop accu (Carets(n+1)::tl)
      | Carets a::Carets b::tl ->
        if trackfix then eprintf "Caret 4\n";
        loop accu (Carets(a+b+2)::tl)
      | Cbrace::Cbrace::tl ->
        if trackfix then eprintf "Cbrace 1\n";
        loop accu (Cbraces 0::tl)
      | Cbraces n::Cbrace::tl ->
        if trackfix then eprintf "Cbrace 2\n";
        loop accu (Cbraces(n+1)::tl)
      | Cbrace::Cbraces n::tl ->
        if trackfix then eprintf "Cbrace 3\n";
        loop accu (Cbraces(n+1)::tl)
      | Cbraces a::Cbraces b::tl ->
        if trackfix then eprintf "Cbrace 4\n";
        loop accu (Cbraces(a+b+2)::tl)
      | Colon::Colon::tl ->
        if trackfix then eprintf "Colon 1\n";
        loop accu (Colons 0::tl)
      | Colons n::Colon::tl ->
        if trackfix then eprintf "Colon 2\n";
        loop accu (Colons(n+1)::tl)
      | Colon::Colons n::tl ->
        if trackfix then eprintf "Colon 3\n";
        loop accu (Colons(n+1)::tl)
      | Colons a::Colons b::tl ->
        if trackfix then eprintf "Colon 4\n";
        loop accu (Colons(a+b+2)::tl)
      | Comma::Comma::tl ->
        if trackfix then eprintf "Comma 1\n";
        loop accu (Commas 0::tl)
      | Commas n::Comma::tl ->
        if trackfix then eprintf "Comma 2\n";
        loop accu (Commas(n+1)::tl)
      | Comma::Commas n::tl ->
        if trackfix then eprintf "Comma 3\n";
        loop accu (Commas(n+1)::tl)
      | Commas a::Commas b::tl ->
        if trackfix then eprintf "Comma 4\n";
        loop accu (Commas(a+b+2)::tl)
      | Cparenthesis::Cparenthesis::tl ->
        if trackfix then eprintf "Cparenthesis 1\n";
        loop accu (Cparenthesiss 0::tl)
      | Cparenthesiss n::Cparenthesis::tl ->
        if trackfix then eprintf "Cparenthesis 2\n";
        loop accu (Cparenthesiss(n+1)::tl)
      | Cparenthesis::Cparenthesiss n::tl ->
        if trackfix then eprintf "Cparenthesis 3\n";
        loop accu (Cparenthesiss(n+1)::tl)
      | Cparenthesiss a::Cparenthesiss b::tl ->
        if trackfix then eprintf "Cparenthesis 4\n";
        loop accu (Cparenthesiss(a+b+2)::tl)
      | Cbracket::Cbracket::tl ->
        if trackfix then eprintf "Cbracket 1\n";
        loop accu (Cbrackets 0::tl)
      | Cbrackets n::Cbracket::tl ->
        if trackfix then eprintf "Cbracket 2\n";
        loop accu (Cbrackets(n+1)::tl)
      | Cbracket::Cbrackets n::tl ->
        if trackfix then eprintf "Cbracket 3\n";
        loop accu (Cbrackets(n+1)::tl)
      | Cbrackets a::Cbrackets b::tl ->
        if trackfix then eprintf "Cbracket 4\n";
        loop accu (Cbrackets(a+b+2)::tl)
      | Dollar::Dollar::tl ->
        if trackfix then eprintf "Dollar 1\n";
        loop accu (Dollars 0::tl)
      | Dollars n::Dollar::tl ->
        if trackfix then eprintf "Dollar 2\n";
        loop accu (Dollars(n+1)::tl)
      | Dollar::Dollars n::tl ->
        if trackfix then eprintf "Dollar 3\n";
        loop accu (Dollars(n+1)::tl)
      | Dollars a::Dollars b::tl ->
        if trackfix then eprintf "Dollar 4\n";
        loop accu (Dollars(a+b+2)::tl)
      | Dot::Dot::tl ->
        if trackfix then eprintf "Dot 1\n";
        loop accu (Dots 0::tl)
      | Dots n::Dot::tl ->
        if trackfix then eprintf "Dot 2\n";
        loop accu (Dots(n+1)::tl)
      | Dot::Dots n::tl ->
        if trackfix then eprintf "Dot 3\n";
        loop accu (Dots(n+1)::tl)
      | Dots a::Dots b::tl ->
        if trackfix then eprintf "Dot 4\n";
        loop accu (Dots(a+b+2)::tl)
      | Doublequote::Doublequote::tl ->
        if trackfix then eprintf "Doublequote 1\n";
        loop accu (Doublequotes 0::tl)
      | Doublequotes n::Doublequote::tl ->
        if trackfix then eprintf "Doublequote 2\n";
        loop accu (Doublequotes(n+1)::tl)
      | Doublequote::Doublequotes n::tl ->
        if trackfix then eprintf "Doublequote 3\n";
        loop accu (Doublequotes(n+1)::tl)
      | Doublequotes a::Doublequotes b::tl ->
        if trackfix then eprintf "Doublequote 4\n";
        loop accu (Doublequotes(a+b+2)::tl)
      | Exclamation::Exclamation::tl ->
        if trackfix then eprintf "Exclamation 1\n";
        loop accu (Exclamations 0::tl)
      | Exclamations n::Exclamation::tl ->
        if trackfix then eprintf "Exclamation 2\n";
        loop accu (Exclamations(n+1)::tl)
      | Exclamation::Exclamations n::tl ->
        if trackfix then eprintf "Exclamation 3\n";
        loop accu (Exclamations(n+1)::tl)
      | Exclamations a::Exclamations b::tl ->
        if trackfix then eprintf "Exclamation 4\n";
        loop accu (Exclamations(a+b+2)::tl)
      | Equal::Equal::tl ->
        if trackfix then eprintf "Equal 1\n";
        loop accu (Equals 0::tl)
      | Equals n::Equal::tl ->
        if trackfix then eprintf "Equal 2\n";
        loop accu (Equals(n+1)::tl)
      | Equal::Equals n::tl ->
        if trackfix then eprintf "Equal 3\n";
        loop accu (Equals(n+1)::tl)
      | Equals a::Equals b::tl ->
        if trackfix then eprintf "Equal 4\n";
        loop accu (Equals(a+b+2)::tl)
      | Greaterthan::Greaterthan::tl ->
        if trackfix then eprintf "Greaterthan 1\n";
        loop accu (Greaterthans 0::tl)
      | Greaterthans n::Greaterthan::tl ->
        if trackfix then eprintf "Greaterthan 2\n";
        loop accu (Greaterthans(n+1)::tl)
      | Greaterthan::Greaterthans n::tl ->
        if trackfix then eprintf "Greaterthan 3\n";
        loop accu (Greaterthans(n+1)::tl)
      | Greaterthans a::Greaterthans b::tl ->
        if trackfix then eprintf "Greaterthan 4\n";
        loop accu (Greaterthans(a+b+2)::tl)
      | Hash::Hash::tl ->
        if trackfix then eprintf "Hash 1\n";
        loop accu (Hashs 0::tl)
      | Hashs n::Hash::tl ->
        if trackfix then eprintf "Hash 2\n";
        loop accu (Hashs(n+1)::tl)
      | Hash::Hashs n::tl ->
        if trackfix then eprintf "Hash 3\n";
        loop accu (Hashs(n+1)::tl)
      | Hashs a::Hashs b::tl ->
        if trackfix then eprintf "Hash 4\n";
        loop accu (Hashs(a+b+2)::tl)
      | Lessthan::Lessthan::tl ->
        if trackfix then eprintf "Lessthan 1\n";
        loop accu (Lessthans 0::tl)
      | Lessthans n::Lessthan::tl ->
        if trackfix then eprintf "Lessthan 2\n";
        loop accu (Lessthans(n+1)::tl)
      | Lessthan::Lessthans n::tl ->
        if trackfix then eprintf "Lessthan 3\n";
        loop accu (Lessthans(n+1)::tl)
      | Lessthans a::Lessthans b::tl ->
        if trackfix then eprintf "Lessthan 4\n";
        loop accu (Lessthans(a+b+2)::tl)
      | Minus::Minus::tl ->
        if trackfix then eprintf "Minus 1\n";
        loop accu (Minuss 0::tl)
      | Minuss n::Minus::tl ->
        if trackfix then eprintf "Minus 2\n";
        loop accu (Minuss(n+1)::tl)
      | Minus::Minuss n::tl ->
        if trackfix then eprintf "Minus 3\n";
        loop accu (Minuss(n+1)::tl)
      | Minuss a::Minuss b::tl ->
        if trackfix then eprintf "Minus 4\n";
        loop accu (Minuss(a+b+2)::tl)
      | Newline::Newline::tl ->
        if trackfix then eprintf "Newline 1\n";
        loop accu (Newlines 0::tl)
      | Newlines n::Newline::tl ->
        if trackfix then eprintf "Newline 2\n";
        loop accu (Newlines(n+1)::tl)
      | Newline::Newlines n::tl ->
        if trackfix then eprintf "Newline 3\n";
        loop accu (Newlines(n+1)::tl)
      | Newlines a::Newlines b::tl ->
        if trackfix then eprintf "Newline 4\n";
        loop accu (Newlines(a+b+2)::tl)
      | Obrace::Obrace::tl ->
        if trackfix then eprintf "Obrace 1\n";
        loop accu (Obraces 0::tl)
      | Obraces n::Obrace::tl ->
        if trackfix then eprintf "Obrace 2\n";
        loop accu (Obraces(n+1)::tl)
      | Obrace::Obraces n::tl ->
        if trackfix then eprintf "Obrace 3\n";
        loop accu (Obraces(n+1)::tl)
      | Obraces a::Obraces b::tl ->
        if trackfix then eprintf "Obrace 4\n";
        loop accu (Obraces(a+b+2)::tl)
      | Oparenthesis::Oparenthesis::tl ->
        if trackfix then eprintf "Oparenthesis 1\n";
        loop accu (Oparenthesiss 0::tl)
      | Oparenthesiss n::Oparenthesis::tl ->
        if trackfix then eprintf "Oparenthesis 2\n";
        loop accu (Oparenthesiss(n+1)::tl)
      | Oparenthesis::Oparenthesiss n::tl ->
        if trackfix then eprintf "Oparenthesis 3\n";
        loop accu (Oparenthesiss(n+1)::tl)
      | Oparenthesiss a::Oparenthesiss b::tl ->
        if trackfix then eprintf "Oparenthesis 4\n";
        loop accu (Oparenthesiss(a+b+2)::tl)
      | Obracket::Obracket::tl ->
        if trackfix then eprintf "Obracket 1\n";
        loop accu (Obrackets 0::tl)
      | Obrackets n::Obracket::tl ->
        if trackfix then eprintf "Obracket 2\n";
        loop accu (Obrackets(n+1)::tl)
      | Obracket::Obrackets n::tl ->
        if trackfix then eprintf "Obracket 3\n";
        loop accu (Obrackets(n+1)::tl)
      | Obrackets a::Obrackets b::tl ->
        if trackfix then eprintf "Obracket 4\n";
        loop accu (Obrackets(a+b+2)::tl)
      | Percent::Percent::tl ->
        if trackfix then eprintf "Percent 1\n";
        loop accu (Percents 0::tl)
      | Percents n::Percent::tl ->
        if trackfix then eprintf "Percent 2\n";
        loop accu (Percents(n+1)::tl)
      | Percent::Percents n::tl ->
        if trackfix then eprintf "Percent 3\n";
        loop accu (Percents(n+1)::tl)
      | Percents a::Percents b::tl ->
        if trackfix then eprintf "Percent 4\n";
        loop accu (Percents(a+b+2)::tl)
      | Plus::Plus::tl ->
        if trackfix then eprintf "Plus 1\n";
        loop accu (Pluss 0::tl)
      | Pluss n::Plus::tl ->
        if trackfix then eprintf "Plus 2\n";
        loop accu (Pluss(n+1)::tl)
      | Plus::Pluss n::tl ->
        if trackfix then eprintf "Plus 3\n";
        loop accu (Pluss(n+1)::tl)
      | Pluss a::Pluss b::tl ->
        if trackfix then eprintf "Plus 4\n";
        loop accu (Pluss(a+b+2)::tl)
      | Question::Question::tl ->
        if trackfix then eprintf "Question 1\n";
        loop accu (Questions 0::tl)
      | Questions n::Question::tl ->
        if trackfix then eprintf "Question 2\n";
        loop accu (Questions(n+1)::tl)
      | Question::Questions n::tl ->
        if trackfix then eprintf "Question 3\n";
        loop accu (Questions(n+1)::tl)
      | Questions a::Questions b::tl ->
        if trackfix then eprintf "Question 4\n";
        loop accu (Questions(a+b+2)::tl)
      | Quote::Quote::tl ->
        if trackfix then eprintf "Quote 1\n";
        loop accu (Quotes 0::tl)
      | Quotes n::Quote::tl ->
        if trackfix then eprintf "Quote 2\n";
        loop accu (Quotes(n+1)::tl)
      | Quote::Quotes n::tl ->
        if trackfix then eprintf "Quote 3\n";
        loop accu (Quotes(n+1)::tl)
      | Quotes a::Quotes b::tl ->
        if trackfix then eprintf "Quote 4\n";
        loop accu (Quotes(a+b+2)::tl)
      | Semicolon::Semicolon::tl ->
        if trackfix then eprintf "Semicolon 1\n";
        loop accu (Semicolons 0::tl)
      | Semicolons n::Semicolon::tl ->
        if trackfix then eprintf "Semicolon 2\n";
        loop accu (Semicolons(n+1)::tl)
      | Semicolon::Semicolons n::tl ->
        if trackfix then eprintf "Semicolon 3\n";
        loop accu (Semicolons(n+1)::tl)
      | Semicolons a::Semicolons b::tl ->
        if trackfix then eprintf "Semicolon 4\n";
        loop accu (Semicolons(a+b+2)::tl)
      | Slash::Slash::tl ->
        if trackfix then eprintf "Slash 1\n";
        loop accu (Slashs 0::tl)
      | Slashs n::Slash::tl ->
        if trackfix then eprintf "Slash 2\n";
        loop accu (Slashs(n+1)::tl)
      | Slash::Slashs n::tl ->
        if trackfix then eprintf "Slash 3\n";
        loop accu (Slashs(n+1)::tl)
      | Slashs a::Slashs b::tl ->
        if trackfix then eprintf "Slash 4\n";
        loop accu (Slashs(a+b+2)::tl)
      | Space::Space::tl ->
        if trackfix then eprintf "Space 1\n";
        loop accu (Spaces 0::tl)
      | Spaces n::Space::tl ->
        if trackfix then eprintf "Space 2\n";
        loop accu (Spaces(n+1)::tl)
      | Space::Spaces n::tl ->
        if trackfix then eprintf "Space 3\n";
        loop accu (Spaces(n+1)::tl)
      | Spaces a::Spaces b::tl ->
        if trackfix then eprintf "Space 4\n";
        loop accu (Spaces(a+b+2)::tl)
      | Star::Star::tl ->
        if trackfix then eprintf "Star 1\n";
        loop accu (Stars 0::tl)
      | Stars n::Star::tl ->
        if trackfix then eprintf "Star 2\n";
        loop accu (Stars(n+1)::tl)
      | Star::Stars n::tl ->
        if trackfix then eprintf "Star 3\n";
        loop accu (Stars(n+1)::tl)
      | Stars a::Stars b::tl ->
        if trackfix then eprintf "Star 4\n";
        loop accu (Stars(a+b+2)::tl)
      | Tab::Tab::tl ->
        if trackfix then eprintf "Tab 1\n";
        loop accu (Tabs 0::tl)
      | Tabs n::Tab::tl ->
        if trackfix then eprintf "Tab 2\n";
        loop accu (Tabs(n+1)::tl)
      | Tab::Tabs n::tl ->
        if trackfix then eprintf "Tab 3\n";
        loop accu (Tabs(n+1)::tl)
      | Tabs a::Tabs b::tl ->
        if trackfix then eprintf "Tab 4\n";
        loop accu (Tabs(a+b+2)::tl)
      | Tilde::Tilde::tl ->
        if trackfix then eprintf "Tilde 1\n";
        loop accu (Tildes 0::tl)
      | Tildes n::Tilde::tl ->
        if trackfix then eprintf "Tilde 2\n";
        loop accu (Tildes(n+1)::tl)
      | Tilde::Tildes n::tl ->
        if trackfix then eprintf "Tilde 3\n";
        loop accu (Tildes(n+1)::tl)
      | Tildes a::Tildes b::tl ->
        if trackfix then eprintf "Tilde 4\n";
        loop accu (Tildes(a+b+2)::tl)
      | Underscore::Underscore::tl ->
        if trackfix then eprintf "Underscore 1\n";
        loop accu (Underscores 0::tl)
      | Underscores n::Underscore::tl ->
        if trackfix then eprintf "Underscore 2\n";
        loop accu (Underscores(n+1)::tl)
      | Underscore::Underscores n::tl ->
        if trackfix then eprintf "Underscore 3\n";
        loop accu (Underscores(n+1)::tl)
      | Underscores a::Underscores b::tl ->
        if trackfix then eprintf "Underscore 4\n";
        loop accu (Underscores(a+b+2)::tl)| x::tl -> loop (x::accu) tl
      | [] -> List.rev accu
    in
    loop [] l


  (* Remove all [NL] and [Br] at the beginning. *)
  let rec remove_initial_newlines = function
    | [] -> []
    | (NL | Br) :: tl -> remove_initial_newlines tl
    | l -> l


  (** - recognizes paragraphs
      - glues following blockquotes  *)
  let make_paragraphs md =
    let rec loop cp accu = function (* cp means current paragraph *)
      | [] ->
        let accu =
          match cp with
          | [] | [NL] | [Br] -> accu
          | (NL|Br)::cp -> Paragraph(List.rev cp)::accu
          | cp -> Paragraph(List.rev cp)::accu
        in
        List.rev accu
      | Blockquote b1 :: Blockquote b2 :: tl ->
        loop cp accu (Blockquote(b1@b2):: tl)
      | Blockquote b1 :: (NL|Br as x) :: Blockquote b2 :: tl ->
        loop cp accu (Blockquote(b1@(x::b2)):: tl)
      | Blockquote b :: tl ->
        let e = Blockquote(loop [] [] b) in
        (match cp with
         | [] | [NL] | [Br] -> loop cp (e::accu) tl
         | _ -> loop [] (e::Paragraph(List.rev cp)::accu) tl)
      | (Ulp b) :: tl ->
        let e = Ulp(List.map (fun li -> loop [] [] li) b) in
        (match cp with
         | [] | [NL] | [Br] -> loop cp (e::accu) tl
         | _ -> loop [] (e::Paragraph(List.rev cp)::accu) tl)
      | (Olp b) :: tl ->
        let e = Olp(List.map (fun li -> loop [] [] li) b) in
        (match cp with
         | [] | [NL] | [Br] -> loop cp (e::accu) tl
         | _ -> loop [] (e::Paragraph(List.rev cp)::accu) tl)
      | Html_comment _ as e :: tl ->
        (match cp with
         | [] -> loop [] (e::accu) tl
         | [NL] | [Br] -> loop [] (e::NL::accu) tl
         | _ -> loop (e::cp) accu tl)
      | (Raw_block _ | Code_block _ | H1 _ | H2 _ | H3 _ | H4 _ | H5 _ | H6 _ | Ol _ | Ul _
        | Html_block _) as e :: tl ->
        (match cp with
         | [] | [NL] | [Br] -> loop cp (e::accu) tl
         | _ -> loop [] (e::Paragraph(List.rev cp)::accu) tl)
      | Text "\n" :: _ | Paragraph _ :: _ ->
        invalid_arg "Omd_backend.make_paragraphs"
      | (NL|Br) :: (NL|Br) :: tl ->
        let tl = remove_initial_newlines tl in
        begin match cp with
          | [] | [NL] | [Br] -> loop [] (NL::accu) tl
          | _ -> loop [] (Paragraph(List.rev cp)::accu) tl
        end
      | x::tl ->
        loop (x::cp) accu tl
    in
    let remove_white_crumbs l =
      let rec loop = function
        | [] -> []
        | Text " " :: tl
        | NL::tl
        | Br::tl
          ->
          loop tl
        | l -> l
      in
      List.rev (loop (List.rev l))
    in
    let rec clean_paragraphs = function
      | [] -> []
      | Paragraph[]::tl -> tl
      | Paragraph(p) :: tl ->
        Paragraph(clean_paragraphs
                    (remove_initial_newlines
                       (remove_white_crumbs(normalise_md p))))
        :: clean_paragraphs tl
      | H1 v :: tl -> H1(clean_paragraphs v)
                      :: clean_paragraphs tl
      | H2 v :: tl -> H2(clean_paragraphs v)
                      :: clean_paragraphs tl
      | H3 v :: tl -> H3(clean_paragraphs v)
                      :: clean_paragraphs tl
      | H4 v :: tl -> H4(clean_paragraphs v)
                      :: clean_paragraphs tl
      | H5 v :: tl -> H5(clean_paragraphs v)
                      :: clean_paragraphs tl
      | H6 v :: tl -> H6(clean_paragraphs v)
                      :: clean_paragraphs tl
      | Emph v :: tl -> Emph(clean_paragraphs v)
                        :: clean_paragraphs tl
      | Bold v :: tl -> Bold(clean_paragraphs v)
                        :: clean_paragraphs tl
      | Ul v :: tl -> Ul(List.map clean_paragraphs v)
                      :: clean_paragraphs tl
      | Ol v :: tl -> Ol(List.map clean_paragraphs v)
                      :: clean_paragraphs tl
      | Ulp v :: tl -> Ulp(List.map clean_paragraphs v)
                       :: clean_paragraphs tl
      | Olp v :: tl -> Olp(List.map clean_paragraphs v)
                       :: clean_paragraphs tl
      | Blockquote v :: tl -> Blockquote(clean_paragraphs v)
                              :: clean_paragraphs tl
      | Url(href,v,title) :: tl -> Url(href,(clean_paragraphs v),title)
                                   :: clean_paragraphs tl
      | Text _
      | Code _
      | Code_block _
      | Br
      | Hr
      | NL
      | Ref _
      | Img_ref _
      | Raw _
      | Raw_block _
      | Html _
      | Html_block _
      | Html_comment _
      | Img _
      | X _ as v :: tl -> v :: clean_paragraphs tl
    in
    clean_paragraphs(loop [] [] md)


  (** [assert_well_formed] is a developer's function that helps to
      track badly constructed token lists.  This function has an
      effect only if [trackfix] is [true].  *)
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
      | [] -> Omd_lexer.string_of_tokens (List.rev accu)
      | e::tl as r ->
        if r == remains then
          Omd_lexer.string_of_tokens (List.rev accu)
        else
          loop (e::accu) tl
    in loop [] l


  let unindent_rev n lexemes =
    if debug then eprintf "CALL: Omd_parser.unindent_rev\n%!";
    assert_well_formed lexemes;
    let rec loop accu cl = function
      | Newlines x::(Space|Spaces _)::Newlines y::tl ->
        loop accu cl (Newlines(x+y+2)::tl)
      | Newline::(Space|Spaces _)::Newlines x::tl ->
        loop accu cl (Newlines(1+x)::tl)
      | Newlines x::(Space|Spaces _)::Newline::tl ->
        loop accu cl (Newlines(1+x)::tl)
      | Newline::(Space|Spaces _)::Newline::tl ->
        loop accu cl (Newlines(0)::tl)

      | (Newline|Newlines 0 as nl)::(Space|Spaces _ as s)::(
          (Number _::Dot::(Space|Spaces _)::_)
        | ((Star|Plus|Minus)::(Space|Spaces _)::_)
          as tl) as l ->
        if n = Omd_lexer.length s then
          loop (nl::cl@accu) [] tl
        else
          (cl@accu), l
      | (Newline|Newlines 0 as nl)::(Space|Spaces _ as s)::tl ->
        let x = Omd_lexer.length s - n in
        loop (nl::cl@accu)
          (if x > 0 then [Omd_lexer.make_space x] else [])
          tl
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
    let fst, snd = unindent_rev n lexemes in
    List.rev fst, snd

  let rec is_blank = function
    | (Space | Spaces _ | Newline | Newlines _) :: tl ->
      is_blank tl
    | [] -> true
    | _ -> false

  let semph_or_bold (n:int) (l:l) =
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
              if Omd_lexer.length s = n then
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

  let sm_uemph_or_bold (n:int) (l:l) =
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
              if Omd_lexer.length s = n then
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


  let gh_uemph_or_bold (n:int) (l:l) =
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
              if Omd_lexer.length s = n then
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
    if gh_uemph_or_bold_style then
      gh_uemph_or_bold n l
    else
      sm_uemph_or_bold n l

  let eat_blank =
    eat (function |Space|Spaces _|Newline|Newlines _ -> true| _ -> false)


  (* used by tag__maybe_h1 and tag__maybe_h2 *)
  let setext_title (l:l) : (Omd_representation.tok list * l) option =
    assert_well_formed l;
    let rec detect_balanced_bqs n r l =
      (* If there's a balanced (complete) backquote-started code block
         then it should be "ignored", else it means the line that
         follows is part of a code block, so it's not defining a
         setext-style title. *)
      if debug then
        eprintf "detect_balanced_bqs n=%d r=%S l=%S\n%!"
          n (Omd_lexer.string_of_tokens r) (Omd_lexer.string_of_tokens l);
      match l with
      | [] ->
        None
      | (Newline|Newlines _)::_ ->
        None
      | Backslash::Backquote::tl ->
        detect_balanced_bqs n (Backquote::Backslash::r) tl
      | Backslash::Backquotes 0::tl ->
        detect_balanced_bqs n (Backquote::Backslash::r) (Backquote::tl)
      | Backslash::Backquotes x::tl ->
        detect_balanced_bqs n (Backquote::Backslash::r) (Backquotes(x-1)::tl)
      | Backslashs(m) as b::Backquote::tl when m mod 2 = 1 ->
        detect_balanced_bqs n (Backquote::b::r) tl
      | Backslashs(m) as b::Backquotes 0::tl when m mod 2 = 1 ->
        detect_balanced_bqs n (Backquote::b::r) (Backquote::tl)
      | Backslashs(m) as b::Backquotes x::tl when m mod 2 = 1 ->
        detect_balanced_bqs n (Backquote::b::r) (Backquotes(x-1)::tl)
      | (Backquote as b)::tl when n = 1 ->
        Some(List.rev (b::r), tl)
      | (Backquotes x as b)::tl when n = x+2 ->
        Some(List.rev (b::r), tl)
      | e::tl ->
        detect_balanced_bqs n (e::r) tl
    in
    let rec loop r = function
      | [] ->
        if r = [] then
          None
        else
          Some(List.rev r, [])
      | Backslash::Backquote::tl ->
        loop (Backquote::Backslash::r) tl
      | Backslashs(m) as b::Backquote::tl when m mod 2 = 1 ->
        loop (Backquote::b::r) tl
      | Backslash::Backquotes 0::tl ->
        loop (Backquote::Backslash::r) (Backquote::tl)
      | Backslash::Backquotes x::tl ->
        loop (Backquote::Backslash::r) (Backquotes(x-1)::tl)
      | Backslashs(m) as b::Backquotes 0::tl when m mod 2 = 1 ->
        loop (Backquote::b::r) (Backquote::tl)
      | Backslashs(m) as b::Backquotes x::tl when m mod 2 = 1 ->
        loop (Backquote::b::r) (Backquotes(x-1)::tl)
      | Backquote::tl ->
        begin match detect_balanced_bqs 1 [] tl with
          | Some(bl,tl) -> loop (bl@r) tl
          | _ -> None
        end
      | Backquotes(x)::tl ->
        begin match detect_balanced_bqs (x+2) [] tl with
          | Some(bl,tl) -> loop (bl@r) tl
          | _ -> None
        end
      | Newline::(Equal|Equals _|Minus|Minuss _)::tl ->
        if r = [] then
          None
        else
          Some(List.rev r, tl)
      | (Newline|Newlines _)::_ ->
        if debug then eprintf "Omd_parser.setext_title is wrongly used!\n%!";
        None
      | e::tl ->
        loop (e::r) tl
    in
    let result = loop [] l in
    if debug then
      eprintf "setext_title l=%S result=%S,%S\n%!"
        (Omd_lexer.string_of_tokens l)
        (match result with None -> ""
                         | Some (x,tl) -> Omd_lexer.string_of_tokens x)
        (match result with None -> ""
                         | Some (x,tl) -> Omd_lexer.string_of_tokens tl);
    result


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
            eprintf "Warning: Omd_parser.tag__maybe_h1 is wrongly used \
                     (p=%S)!\n"
              (Omd_lexer.string_of_tokens p);
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
            eprintf "Warning: Omd_parser.tag__maybe_h2 is wrongly used \
                     (p=%S)!\n"
              (Omd_lexer.string_of_tokens p);
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

  let hr l =
    match hr_m l with
    | None -> hr_s l
    | Some _ as tl -> tl

  (** [bcode] parses code that's delimited by backquote(s) *)
  let bcode ?(default_lang=default_lang) r p l =
    assert_well_formed l;
    let e, tl =
      match l with
      | (Backquote|Backquotes _ as e)::tl -> e, tl
      | _ -> failwith "Omd_parser.bcode is wrongly called"
    in
    let rec code_block accu = function
      | [] ->
        None
      | Backquote::tl ->
        if e = Backquote then
          match accu with
          | Newline::accu ->
            Some(List.rev accu, tl)
          | _ ->
            Some(List.rev accu, tl)
        else
          code_block (Backquote::accu) tl
      | (Backquotes n as b)::tl ->
        if e = b then
          match accu with
          | Newline::accu ->
            Some(List.rev accu, tl)
          | _ ->
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
      && (match p with []|[Newline|Newlines _] -> true | _ -> false)
      && (match e with Backquotes n when n > 0 -> true | _ -> false)
      then
        match cb with
        | Word lang :: (Space|Spaces _) :: Newline :: tl
        | Word lang :: Newline :: tl ->
          let code = Omd_lexer.string_of_tokens tl in
          Some(Code_block(lang, code) :: r, [Backquote], l)
        | Word lang :: (Space|Spaces _) :: Newlines 0 :: tl
        | Word lang :: Newlines 0 :: tl ->
          let code = Omd_lexer.string_of_tokens(Newline::tl) in
          Some(Code_block(lang, code) :: r, [Backquote], l)
        | Word lang :: (Space|Spaces _) :: Newlines n :: tl
        | Word lang :: Newlines n :: tl ->
          let code = Omd_lexer.string_of_tokens (Newlines(n-1)::tl) in
          Some(Code_block(lang, code) :: r, [Backquote], l)
        | Newline :: tl ->
          let code = Omd_lexer.string_of_tokens  tl in
          Some(Code_block(default_lang, code) :: r, [Backquote], l)
        | _ ->
          let code = Omd_lexer.string_of_tokens cb in
          Some(Code_block(default_lang, code) :: r, [Backquote], l)
      else
        let clean_bcode s =
          let rec loop1 i =
            if i = String.length s then 0
            else match s.[i] with
              | ' ' -> loop1(i+1)
              | _ -> i
          in
          let rec loop2 i =
            if i = -1 then String.length s
            else match s.[i] with
              | ' ' -> loop2(i-1)
              | _ -> i+1
          in
          match loop1 0, loop2 (String.length s - 1) with
          | 0, n when n = String.length s - 1 -> s
          | i, n -> String.sub s i (n-i)
        in
        let code = Omd_lexer.string_of_tokens cb in
        if debug then
          eprintf "clean_bcode %S => %S\n%!" code (clean_bcode code);
        Some(Code(default_lang, clean_bcode code) :: r, [Backquote], l)


  exception NL_exception
  exception Premature_ending

  (* !!DO NOT DELETE THIS!! TODO: add ability to read code (github issue #85)
     The program that generates the generated part that follows right after.
     List.iter (fun (a,b,c) ->
     print_endline ("let read_until_"^a^" ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
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
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
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
     in
     if debug then 
       eprintf \"Omd_parser.read_until_"^a^" %S bq=%b no_nl=%b\\n%!\" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf \"Omd_parser.read_until_"^a^" %S bq=%b no_nl=%b => %S\\n%!\" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
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
     ]
  *)

  (* begin generated part *)
let read_until_gt ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Greaterthan as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Greaterthans 0) :: tl ->
        loop (Greaterthan::Backslash::accu) n (Greaterthan::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
     
      | (Backslash as a) :: (Lessthan as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Lessthans 0) :: tl ->
        loop (Lessthan::Backslash::accu) n (Lessthan::tl)
      | Lessthan as e :: tl ->
        loop (e::accu) (n+1) tl
      | Lessthans x as e :: tl ->
        loop (e::accu) (n+x+2) tl
         | Greaterthan as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Greaterthans 0 :: tl ->
        if n = 0 then
          List.rev accu, Greaterthan::tl
        else
          loop (Greaterthan::accu) (n-1) (Greaterthan::tl)
      | Greaterthans n :: tl ->
        List.rev accu, Greaterthans(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_gt %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_gt %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_lt ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Lessthan as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Lessthans 0) :: tl ->
        loop (Lessthan::Backslash::accu) n (Lessthan::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
         | Lessthan as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Lessthans 0 :: tl ->
        if n = 0 then
          List.rev accu, Lessthan::tl
        else
          loop (Lessthan::accu) (n-1) (Lessthan::tl)
      | Lessthans n :: tl ->
        List.rev accu, Lessthans(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_lt %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_lt %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_cparenth ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Cparenthesis as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Cparenthesiss 0) :: tl ->
        loop (Cparenthesis::Backslash::accu) n (Cparenthesis::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
     
      | (Backslash as a) :: (Oparenthesis as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Oparenthesiss 0) :: tl ->
        loop (Oparenthesis::Backslash::accu) n (Oparenthesis::tl)
      | Oparenthesis as e :: tl ->
        loop (e::accu) (n+1) tl
      | Oparenthesiss x as e :: tl ->
        loop (e::accu) (n+x+2) tl
         | Cparenthesis as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Cparenthesiss 0 :: tl ->
        if n = 0 then
          List.rev accu, Cparenthesis::tl
        else
          loop (Cparenthesis::accu) (n-1) (Cparenthesis::tl)
      | Cparenthesiss n :: tl ->
        List.rev accu, Cparenthesiss(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_cparenth %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_cparenth %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_oparenth ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Oparenthesis as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Oparenthesiss 0) :: tl ->
        loop (Oparenthesis::Backslash::accu) n (Oparenthesis::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
         | Oparenthesis as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Oparenthesiss 0 :: tl ->
        if n = 0 then
          List.rev accu, Oparenthesis::tl
        else
          loop (Oparenthesis::accu) (n-1) (Oparenthesis::tl)
      | Oparenthesiss n :: tl ->
        List.rev accu, Oparenthesiss(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_oparenth %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_oparenth %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_dq ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Doublequote as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Doublequotes 0) :: tl ->
        loop (Doublequote::Backslash::accu) n (Doublequote::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
         | Doublequote as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Doublequotes 0 :: tl ->
        if n = 0 then
          List.rev accu, Doublequote::tl
        else
          loop (Doublequote::accu) (n-1) (Doublequote::tl)
      | Doublequotes n :: tl ->
        List.rev accu, Doublequotes(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_dq %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_dq %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_q ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Quote as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Quotes 0) :: tl ->
        loop (Quote::Backslash::accu) n (Quote::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
         | Quote as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Quotes 0 :: tl ->
        if n = 0 then
          List.rev accu, Quote::tl
        else
          loop (Quote::accu) (n-1) (Quote::tl)
      | Quotes n :: tl ->
        List.rev accu, Quotes(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_q %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_q %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_obracket ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Obracket as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Obrackets 0) :: tl ->
        loop (Obracket::Backslash::accu) n (Obracket::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
         | Obracket as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Obrackets 0 :: tl ->
        if n = 0 then
          List.rev accu, Obracket::tl
        else
          loop (Obracket::accu) (n-1) (Obracket::tl)
      | Obrackets n :: tl ->
        List.rev accu, Obrackets(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_obracket %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_obracket %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_cbracket ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Cbracket as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Cbrackets 0) :: tl ->
        loop (Cbracket::Backslash::accu) n (Cbracket::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
     
      | (Backslash as a) :: (Obracket as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Obrackets 0) :: tl ->
        loop (Obracket::Backslash::accu) n (Obracket::tl)
      | Obracket as e :: tl ->
        loop (e::accu) (n+1) tl
      | Obrackets x as e :: tl ->
        loop (e::accu) (n+x+2) tl
         | Cbracket as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Cbrackets 0 :: tl ->
        if n = 0 then
          List.rev accu, Cbracket::tl
        else
          loop (Cbracket::accu) (n-1) (Cbracket::tl)
      | Cbrackets n :: tl ->
        List.rev accu, Cbrackets(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_cbracket %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_cbracket %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
     
let read_until_space ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | (Backslash as a) :: (Space as b) :: tl ->
        loop (b::a::accu) n tl
      | Backslash :: (Spaces 0) :: tl ->
        loop (Space::Backslash::accu) n (Space::tl)
      | (Backslashs 0 as e) :: tl ->
        loop (e::accu) n tl
      | (Backslashs x as e) :: tl ->
        if x mod 2 = 0 then
          loop (e::accu) n tl
        else
          loop (Backslashs(x-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(Omd_lexer.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl
         | Space as e :: tl ->
        if n = 0 then
          List.rev accu, tl
        else
          loop (e::accu) (n-1) tl
      | Spaces 0 :: tl ->
        if n = 0 then
          List.rev accu, Space::tl
        else
          loop (Space::accu) (n-1) (Space::tl)
      | Spaces n :: tl ->
        List.rev accu, Spaces(n-1)::tl
      | (Newline|Newlines _ as e)::tl ->
        if no_nl then
          raise NL_exception
        else
          loop (e::accu) n tl
      | e::tl ->
        loop (e::accu) n tl
      | [] ->
        raise Premature_ending
     in
     if debug then 
       eprintf "Omd_parser.read_until_space %S bq=%b no_nl=%b\n%!" (Omd_lexer.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then 
       eprintf "Omd_parser.read_until_space %S bq=%b no_nl=%b => %S\n%!" (Omd_lexer.string_of_tokens l) bq no_nl (Omd_lexer.string_of_tokens (fst res));
     res
  (* /end generated part *)

  let read_until_newline l =
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

  (* H1, H2, H3, ... *)
  let read_title main_loop n r _previous lexemes =
    let title, rest =
      let rec loop accu = function
        | (Hash|Hashs _) :: ((Newline|Newlines _) :: _ as l)
        | (Hash|Hashs _) :: (Space|Spaces _) :: ((Newline|Newlines _)::_ as l)
        | ((Newline|Newlines _) :: _ as l)
        | ([] as l)
        | (Space|Spaces _) :: (Hash|Hashs _) :: ((Newline|Newlines _) :: _ as l)
        | (Space|Spaces _) :: (Hash|Hashs _) :: (Space|Spaces _)
          :: ((Newline|Newlines _)::_ as l)
        | (Space|Spaces _) :: ((Newline|Newlines _) :: _ as l)
        | (Space|Spaces _) :: ([] as l) ->
          main_loop [] [] (List.rev accu), l
        | [Hash|Hashs _]
        | [(Space|Spaces _); Hash|Hashs _]
        | [(Space|Spaces _); (Hash|Hashs _); (Space|Spaces _)] ->
          main_loop [] [] (List.rev accu), []
        | (Hash|Hashs _ as x) :: tl ->
          loop (Word(Omd_lexer.string_of_token x)::accu) tl
        | x::tl ->
          loop (x::accu) tl
      in
      loop [] lexemes
    in
    match n with
    | 1 -> Some(H1 title :: r, [Newline], rest)
    | 2 -> Some(H2 title :: r, [Newline], rest)
    | 3 -> Some(H3 title :: r, [Newline], rest)
    | 4 -> Some(H4 title :: r, [Newline], rest)
    | 5 -> Some(H5 title :: r, [Newline], rest)
    | 6 -> Some(H6 title :: r, [Newline], rest)
    | _ -> None

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
    | (Newline|Newlines _)::block, tl ->
      if debug then
        eprintf "Omd_parser.emailstyle_quoting %S\n%!"
          (Omd_lexer.string_of_tokens block);
      Some((Blockquote(main_loop [] [] block)::r), [Newline], tl)
    | _ ->
      None


  (* maybe a reference *)
  let maybe_reference rc r _p l =
    assert_well_formed l;
    (* this function is called when we know it's not a link although
       it started with a '[' *)
    (* So it could be a reference or a link definition. *)
    let rec maybe_ref l =
      let text, remains = read_until_cbracket ~bq:true l in
      (* check that there is no ill-placed open bracket *)
      if (try ignore(read_until_obracket ~bq:true text); true
          with Premature_ending -> false) then
        raise Premature_ending; (* <-- ill-placed open bracket *)
      let blank, remains = read_until_obracket ~bq:true remains in
      (* check that there are no unwanted characters between CB and OB. *)
      if eat (let flag = ref true in
              function (* allow only a space, multiple spaces, or a newline *)
              | Newline -> !flag && (flag := false; true)
              | (Space|Spaces _) -> !flag && (flag := false; true)
              | _ -> false) blank <> [] then
        raise Premature_ending (* <-- not a regular reference *)
      else
        match read_until_cbracket ~bq:true remains with
        | [], remains ->
          let fallback = extract_fallback remains (Obracket::l) in
          let id = Omd_lexer.string_of_tokens text in (* implicit anchor *)
          Some(((Ref(rc, id, id, fallback))::r), [Cbracket], remains)
        | id, remains ->
          let fallback = extract_fallback remains (Obracket::l) in
          Some(((Ref(rc, Omd_lexer.string_of_tokens id,
                     Omd_lexer.string_of_tokens text, fallback))::r),
               [Cbracket], remains)
    in
    let rec maybe_nonregular_ref l =
      let text, remains = read_until_cbracket ~bq:true l in
      (* check that there is no ill-placed open bracket *)
      if (try ignore(read_until_obracket ~bq:true text); true
          with Premature_ending -> false) then
        raise Premature_ending; (* <-- ill-placed open bracket *)
      let fallback = extract_fallback remains (Obracket::l) in
      let id = Omd_lexer.string_of_tokens text in (* implicit anchor *)
      Some(((Ref(rc, id, id, fallback))::r), [Cbracket], remains)
    in
    let rec maybe_def l =
      match read_until_cbracket ~bq:true l with
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
              | Doublequote::tl -> read_until_dq ~bq:true tl
              | Quotes(0)::tl -> [], tl
              | Quote::tl -> read_until_q ~bq:true tl
              | Oparenthesis::tl-> read_until_cparenth ~bq:true tl
              | l -> [], l
            in
            let url =
              let url = Omd_lexer.string_of_tokens url in
              if String.length url > 2 && url.[0] = '<'
                 && url.[String.length url - 1] = '>' then
                String.sub url 1 (String.length url - 2)
              else
                url
            in
            rc#add_ref (Omd_lexer.string_of_tokens id)
              (Omd_lexer.string_of_tokens title) url;
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
  let maybe_link main_loop r _p l =
    if debug then eprintf "# maybe_link\n";
    assert_well_formed l;
    let read_url name l =
      if debug then eprintf "# maybe_link>read_url\n";
      try
        let l_cp, r_cp =
          read_until_cparenth ~no_nl:true ~bq:false l
        in
        if debug then eprintf "maybe_link >> l_cp=%S r_cp=%S\n%!"
          (Omd_lexer.string_of_tokens l_cp)
          (Omd_lexer.string_of_tokens r_cp);
        try
          let l_dq, r_dq =
            read_until_dq ~no_nl:true ~bq:false l
          in
          if debug then eprintf "maybe_link >> l_dq=%S r_dq=%S\n%!"
            (Omd_lexer.string_of_tokens l_dq)
            (Omd_lexer.string_of_tokens r_dq);
          (* maybe title *)
          if List.length l_cp > List.length l_dq then (* title *)
            begin
              if debug then eprintf "maybe_link >> title\n%!";
              let url =
                match List.rev l_dq with
                | (Newline|Space|Spaces _)::(Newline|Space|Spaces _)::tl
                | (Newline|Space|Spaces _)::tl ->
                  Omd_lexer.string_of_tokens (List.rev tl)
                | _ ->
                  Omd_lexer.string_of_tokens l_dq
              in
              let title, rest = read_until_dq ~no_nl:false ~bq:false r_dq in
              let rest = snd(read_until_cparenth rest) in
              let title = Omd_lexer.string_of_tokens title in
              Some(Url(url, name, title) :: r, [Cparenthesis], rest)
            end
          else (* no title *)
            raise Premature_ending
        with NL_exception | Premature_ending -> (* no title *)
          begin
            if debug then eprintf "maybe_link >> no title\n%!";
            let url = match List.rev l_cp with
              | (Newline|Space|Spaces _)::(Newline|Space|Spaces _)::tl
              | (Newline|Space|Spaces _)::tl -> List.rev tl
              | _ -> l_cp
            in
            let title, rest = [], r_cp in
            let url = Omd_lexer.string_of_tokens url in
            let title = Omd_lexer.string_of_tokens title in
            Some(Url(url, name, title) :: r, [Cparenthesis], rest)
          end          
      with NL_exception | Premature_ending ->
        None
    in
    let read_name l =
      (* it's not really the "name" of a URL but what
         corresponds to the inner HTML of an HTML 'A' tag *)
      if debug then eprintf "# maybe_link> read_name\n";
      try
        match read_until_cbracket ~bq:true l with
        | name, (Oparenthesis::tl) ->
          read_url (main_loop [] [Obracket] name) (eat_blank tl)
        | _ ->
          None
      with Premature_ending | NL_exception -> None
    in
    read_name l


  let has_paragraphs l =
    (* Has at least 2 consecutive newlines. *)
    List.exists (function Newlines _ -> true | _ -> false) l

  let parse_list main_loop r _p l =
    assert_well_formed l;
    if debug then begin
      eprintf "parse_list r=(%s) p=(%s) l=(%s)\n%!"
        "" (* (Omd_backend.sexpr_of_md (List.rev r)) *)
        "" (* (destring_of_tl p) *)
        (Omd_lexer.destring_of_tokens ~limit:40 l);
    end;
    let module UO = struct type ordered = O | U end in
    let open UO in
    if debug then
      eprintf "parse_list: l=(%s)\n%!" (Omd_lexer.destring_of_tokens l);
    let end_of_item (indent:int) l : tok split_action  = match l with
      | [] ->
        Split([],[])
      | Newlines 0 :: ((Spaces n) :: Greaterthan :: (Space | Spaces _) :: tl
                       as s) ->
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
      | Newline :: (Space as s) :: tl ->
        Continue_with
          ([s;
            Tag(fun r p ->
                function (Space|Spaces _)::tl -> Some(r,p,Space::tl)
                       | _ -> None);
            Newline],
           tl)
      | _::_ ->
        Continue
    in
    let rev_to_t l =
      assert_well_formed l;
      (* Newlines at the end of items have no meaning (except to end the
         item which is expressed by the constructor already). *)
      let l = match l with (Newline | Newlines _) :: tl -> tl | _ -> l in
      main_loop [] [Newline] (List.rev l)
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
      let items = List.rev items in
      match items with
      | (U,_,item)::_ ->
        if p then
          Ulp(List.map (fun (_,_,i) -> i) items)
        else
          Ul(List.map (fun (_,_,i) -> i) items)
      | (O,_,item)::_ ->
        if p then
          Olp(List.map (fun (_,_,i) -> i) items)
        else
          Ol(List.map (fun (_,_,i) -> i) items)
      | [] ->
        failwith "make_up called with []" (* assert false *)
    in
    let rec list_items ~p indents items l =
      if debug then eprintf "list_items: p=%b l=(%s)\n%!"
                            p (Omd_lexer.destring_of_tokens l);
      match l with
      (* no more list items *)
      | [] ->
        make_up p items, l
      (* more list items *)
      (* new unordered items *)
      | (Star|Minus|Plus)::(Space|Spaces _)::tl ->
        begin
          match fsplit_rev ~f:(end_of_item 0) tl with
          | None ->
            make_up p items, l
          | Some(new_item, rest) ->
            let p = p || has_paragraphs new_item in
            if debug then
              eprintf "new_item=%S\n%!" (Omd_lexer.destring_of_tokens new_item);
            match indents with
            | [] ->
              assert(items = []);
              list_items ~p [0] ((U,[0], rev_to_t new_item)::items) rest
            | 0::_ ->
              list_items ~p indents ((U,indents,rev_to_t new_item)::items) rest
            | _::_ ->
              make_up p items, l
        end
      | Space::(Star|Minus|Plus)::(Space|Spaces _)::tl ->
        begin
          match fsplit_rev ~f:(end_of_item 1) tl with
          | None -> make_up p items, l
          | Some(new_item, rest) ->
            let p = p || has_paragraphs new_item in
            match indents with
            | [] ->
              assert(items = []);
              list_items ~p [1] ((U,[1],rev_to_t new_item)::items) rest
             | 1::_ ->
              list_items ~p indents ((U,indents,rev_to_t new_item)::items) rest
            | i::_ ->
              if i > 1 then
                make_up p items, l
              else (* i < 1 : new sub list*)
                let sublist, remains =
                  list_items ~p (1::indents)
                    [(U,1::indents,rev_to_t new_item)] rest
                in
                list_items ~p indents (add sublist items) remains
        end
      | Spaces n::(Star|Minus|Plus)::(Space|Spaces _)::tl ->
        begin
          match fsplit_rev ~f:(end_of_item (n+2)) tl with
          | None ->
            make_up p items, l
          | Some(new_item, rest) ->
            let p = p || has_paragraphs new_item in
            match indents with
            | [] ->
              if debug then
                eprintf "spaces[] l=(%S)\n%!" (Omd_lexer.string_of_tokens l);
              assert(items = []); (* aïe... listes mal formées ?! *)
              list_items ~p [n+2] ((U,[n+2],rev_to_t new_item)::items) rest
            | i::_ ->
              if debug then eprintf "spaces(%d::_) n=%d l=(%S)\n%!"
                  i n (Omd_lexer.string_of_tokens l);
              if i = n + 2 then
                let items = (U,indents,rev_to_t new_item) :: items in
                list_items ~p indents items rest
              else if i < n + 2 then
                let sublist, remains =
                  list_items ~p ((n+2)::indents)
                    [(U,(n+2)::indents,rev_to_t new_item)]
                    rest
                in
                list_items ~p indents (add sublist items) remains
              else (* i > n + 2 *)
                make_up p items, l
        end
      (* new ordered items *)
      | Number _::Dot::(Space|Spaces _)::tl ->
        begin
          match fsplit_rev ~f:(end_of_item 0) tl with
          | None ->
            make_up p items, l
          | Some(new_item, rest) ->
            let p = p || has_paragraphs new_item in
            assert_well_formed new_item;
            match indents with
            | [] ->
              assert(items = []);
              list_items ~p [0] ((O,[0],rev_to_t new_item)::items) rest
            | 0::_ ->
              list_items ~p indents ((O,indents,rev_to_t new_item)::items) rest
            | _::_ ->
              make_up p items, l
        end
      | Space::Number _::Dot::(Space|Spaces _)::tl ->
        begin
          match fsplit_rev ~f:(end_of_item 1) tl with
          | None -> make_up p items, l
          | Some(new_item, rest) ->
            let p = p || has_paragraphs new_item in
            match indents with
            | [] ->
              assert(items = []);
              list_items ~p [1] ((O,[1],rev_to_t new_item)::items) rest
            | 1::_ ->
              list_items ~p indents ((O,indents,rev_to_t new_item)::items) rest
            | i::_ ->
              if i > 1 then
                make_up p items, l
              else (* i < 1 : new sub list*)
                let sublist, remains =
                  list_items ~p (1::indents)
                    [(O,1::indents,rev_to_t new_item)] rest
                in
                list_items ~p:p indents (add sublist items) remains
        end
      | Spaces n::Number _::Dot::(Space|Spaces _)::tl ->
        begin
          match fsplit_rev ~f:(end_of_item (n+2)) tl with
          | None ->
            make_up p items, l
          | Some(new_item, rest) ->
            let p = p || has_paragraphs new_item in
            match indents with
            | [] ->
              if debug then eprintf "spaces[] l=(%S)\n%!"
                  (Omd_lexer.string_of_tokens l);
              assert(items = []); (* aïe... listes mal formées ?! *)
              list_items ~p [n+2] ((O,[n+2],rev_to_t new_item)::items) rest
            | i::_ ->
              if debug then eprintf "spaces(%d::_) n=%d l=(%S)\n%!"
                  i n (Omd_lexer.string_of_tokens l);
              if i = n + 2 then
                list_items ~p indents ((O,indents,rev_to_t new_item)::items) rest
              else if i < n + 2 then
                let sublist, remains =
                  list_items ~p
                    ((n+2)::indents)
                    [(O,(n+2)::indents,rev_to_t new_item)]
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
              | (O,indent::_,item)::tl ->
                 sprintf "(O,%d,%s)" (indent) (Omd_backend.html_of_md item)
                 ^ string_of_items tl
              | (U,indent::_,item)::tl ->
                 sprintf "(U,%d,%s)" (indent) (Omd_backend.html_of_md item)
                 ^ string_of_items tl
              | _ -> "(weird)"
            in
            eprintf "NALI parse_list: l=(%S) items=%s\n%!"
              (Omd_lexer.string_of_tokens l) (string_of_items items)
          end;
        (* not a list item *)
        make_up p items, l
    in
    match list_items ~p:false [] [] l with
    | rp, l ->
      rp::r, [Newline], l



  let icode ?(default_lang=default_lang) r _p l =
    assert_well_formed l;
    (* indented code: returns (r,p,l) where r is the result, p is the
       last thing read, l is the remains *)
    let dummy_tag = Tag(fun r p l -> None) in
    let accu = Buffer.create 42 in
    let rec loop s tl = match s, tl with
      | (Newline|Newlines _ as p), (Space|Spaces(0|1))::_ ->
        (* 1, 2 or 3 spaces. *)
        (* -> Return what's been found as code because what follows isn't. *)
        Code_block(default_lang, Buffer.contents accu) :: r, [p], tl
      | (Newline|Newlines _ as p), Spaces(n)::tl ->
        assert(n>0);
        (* At least 4 spaces, it's still code. *)
        Buffer.add_string accu (Omd_lexer.string_of_token p);
        loop
          (if n >= 4 then Spaces(n-4) else if n = 3 then Space else dummy_tag)
          tl
      | (Newline|Newlines _ as p), (not_spaces::_ as tl) -> (* stop *)
        Code_block(default_lang, Buffer.contents accu) :: r, [p], tl
      (* -> Return what's been found as code because it's no more code. *)
      | p, e::tl ->
        Buffer.add_string accu (Omd_lexer.string_of_token p);
        (* html entities are to be converted later! *)
        loop e tl
      | p, [] ->
        Buffer.add_string accu (Omd_lexer.string_of_token p);
        Code_block(default_lang, Buffer.contents accu)::r, [p], []
    in
    match l with
    | Spaces n::tl ->
      if n >= 4 then
        Some(loop (Spaces(n-4)) tl)
      else if n = 3 then
        Some(loop Space tl)
      else Some(loop dummy_tag tl)
    | _ -> assert false


  (* Returns [(r,p,l)] where [r] is the result, [p] is the last thing
     read, and [l] is what remains. *)
  let spaces_at_beginning_of_line main_loop default_lang n r previous lexemes =
    assert_well_formed lexemes;
    assert (n > 0);
    if n <= 3 then (
      match lexemes with
      | (Star|Minus|Plus) :: (Space|Spaces _) :: _ ->
        (* unordered list *)
        parse_list main_loop r [] (Omd_lexer.make_space n::lexemes)
      | (Number _)::Dot::(Space|Spaces _)::tl ->
        (* ordered list *)
        parse_list main_loop r [] (Omd_lexer.make_space n::lexemes)
      | []
      | (Newline|Newlines _) :: _  -> (* blank line, skip spaces *)
        r, previous, lexemes
      |  _::_ ->
        Text (" ")::r, previous, lexemes
    )
    else ( (* n>=4, blank line or indented code *)
      match lexemes with
      | [] | (Newline|Newlines _) :: _  -> r, previous, lexemes
      | _ ->
        match
          icode ~default_lang r [Newline] (Omd_lexer.make_space n :: lexemes)
        with
        | Some(r,p,l) -> r,p,l
        | None ->
          if debug then
            eprintf "Omd_parser.icode or \
                     Omd_parser.main_loop is broken\n%!";
          assert false
    )

  let spaces_not_at_beginning_of_line n r lexemes =
    assert_well_formed lexemes;
    assert (n > 0);
    if n = 1 then
      (Text " "::r), [Space], lexemes
    else (
      match lexemes with
      | Newline :: tl ->
        (* 2 or more spaces before a newline, eat the newline *)
        Br::r, [Spaces(n-2)], tl
      | Newlines k :: tl ->
        (* 2 or more spaces before a newline, eat 1 newline *)
        let newlines = if k = 0 then Newline else Newlines(k-1) in
        Br::r, [Spaces(n-2)], newlines :: tl
      | _ ->
        assert (n>1);
        (Text (String.make n ' ')::r), [Spaces(n-2)], lexemes
    )


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
            let email = Omd_lexer.string_of_tokens left
                        ^ "@" ^ Omd_lexer.string_of_tokens domain in
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



  let rec main_loop_rev (r:r) (previous:p) (lexemes:l) =
    assert_well_formed lexemes;
    if debug then
      eprintf "main_loop_rev r=%s p=(%s) l=(%s)\n%!"
        (Omd_backend.sexpr_of_md (List.rev r))
        (Omd_lexer.destring_of_tokens previous)
        (Omd_lexer.destring_of_tokens lexemes);
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
            | None ->
              main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
            | Some(r, p, l) ->
              main_loop_rev r p l
          end
        | Some (comments, new_tl) ->
          let r = Html_comment(Omd_lexer.string_of_tokens comments) :: r in
          main_loop_rev r [Greaterthan] new_tl
      end

    (* email-style quoting / blockquote *)
    | ([]|[Newline|Newlines _]), Greaterthan::(Space|Spaces _)::_ ->
      begin
        match
          emailstyle_quoting main_loop r previous (Newline::lexemes)
        with
        | Some(r,p,l) -> main_loop_rev r p l
        | None ->
          if debug then
            eprintf "Omd_parser.emailstyle_quoting or \
                     Omd_parser.main_loop is broken\n%!";
          assert false
      end

    (* email-style quoting, with lines starting with spaces! *)
    | ([]|[Newline|Newlines _]), (Space|Spaces(0|1) as s)
                                 :: Greaterthan :: (Space|Spaces _)::_ ->
      (* It's 1, 2 or 3 spaces, not more because it wouldn't mean
         quoting anymore but code. *)
      begin
        let new_r, p, rest =
          let foo, rest =
            match unindent (Omd_lexer.length s) (Newline::lexemes) with
            | (Newline|Newlines _)::foo, rest -> foo, rest
            | res -> res
          in
          match
            emailstyle_quoting main_loop [] previous (Newline::foo)
          with
          | Some(new_r, p, []) -> new_r, p, rest
          | _ ->
            if debug then
              eprintf "Omd_parser.emailstyle_quoting or \
                       Omd_parser.main_loop is broken\n%!";
            assert false
        in
        main_loop_rev (new_r@r) [Newline] rest
      end

    (* minus *)
    | ([]|[Newline|Newlines _]),
      (Minus|Minuss _ as t) :: ((Space|Spaces _)::_ as tl) ->
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
                | None ->
                  main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
                | Some(r, p, l) ->
                  main_loop_rev r p l
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
            | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
            | Some(r, p, l) -> main_loop_rev r p l
          end
        | Some l -> (* hr *)
          main_loop_rev (Hr::r) [Newline] l
      end

    (* hashes *)
    | ([]|[(Newline|Newlines _)]),
      (Hashs n as t) :: ((Space|Spaces _) :: ttl as tl)
    | ([]|[(Newline|Newlines _)]),
      (Hashs n as t) :: (ttl as tl) -> (* hash titles *)
      if n <= 4 then
        match read_title main_loop (n+2) r previous ttl with
        | Some(r, p, l) -> main_loop_rev r p l
        | None ->
          if debug then
            eprintf "Omd_parser.read_title or \
                     Omd_parser.main_loop is broken\n%!";
          assert false
      else
        begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
          | Some(r, p, l) -> main_loop_rev r p l
        end
    | ([]|[(Newline|Newlines _)]), Hash :: (Space|Spaces _) :: tl
    | ([]|[(Newline|Newlines _)]), Hash :: tl -> (* hash titles *)
      begin match read_title main_loop 1 r previous tl with
        | Some(r, p, l) -> main_loop_rev r p l
        | None ->
          if debug then
            eprintf "Omd_parser.read_title or \
                     Omd_parser.main_loop is broken\n%!";
          assert false
      end
    | _, (Hash|Hashs _ as t) :: tl -> (* hash -- no title *)
      begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
      end

    (* spaces after a newline: could lead to hr *)
    | ([]|[Newline|Newlines _]), ((Space|Spaces _) as sp) :: tl ->
      begin match hr tl with
        | None ->
          (* No [Hr], but maybe [Ul], [Ol], code,... *)
          let n = Omd_lexer.length sp in
          let r, p, l = spaces_at_beginning_of_line
              main_loop default_lang n r previous tl in
          main_loop_rev r p l
        | Some tl ->
          main_loop_rev (Hr::r) [Newline] tl
      end

    (* spaces anywhere *)
    | _, ((Space|Spaces _) as t) :: tl ->
      (* too many cases to be handled here *)
      let n = Omd_lexer.length t in
      let r, p, l = spaces_not_at_beginning_of_line n r tl in
      main_loop_rev r p l

    (* underscores *)
    | _, (Underscore as t) :: tl -> (* one "orphan" underscore, or emph *)
      (match uemph_or_bold 1 tl with
       | None ->
         begin match maybe_extension extensions r previous lexemes with
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
               | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
    | _, Backslash :: (Greaterthan as t) :: tl -> (* \> *)
      main_loop_rev (Text (">") :: r) [t] tl
    | _, Backslash :: Greaterthans 0 :: tl -> (* \>>>... *)
      main_loop_rev (Text (">") :: r) [Backslash; Greaterthan] (Greaterthan :: tl)
    | _, Backslash :: Greaterthans n :: tl -> assert (n >= 0); (* \>>>... *)
      main_loop_rev (Text (">") :: r) [Backslash; Greaterthan]
        (Greaterthans (n-1) :: tl)
    | _, Backslash :: (Lessthan as t) :: tl -> (* \< *)
      main_loop_rev (Text ("<") :: r) [t] tl
    | _, Backslash :: Lessthans 0 :: tl -> (* \<<<... *)
      main_loop_rev (Text ("<") :: r) [Backslash; Lessthan] (Lessthan :: tl)
    | _, Backslash :: Lessthans n :: tl -> assert (n >= 0); (* \<<<... *)
      main_loop_rev (Text ("<") :: r) [Backslash; Lessthan]
        (Lessthans (n-1) :: tl)
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
            ^ (Omd_lexer.string_of_token w) ^ "://"
            ^ (if n = 0 then "" else String.make (n-2) '/')
            ^ Omd_lexer.string_of_tokens (List.rev accu)
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
            | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
      begin match bcode ~default_lang r previous lexemes with
        | Some(r, p, l) -> main_loop_rev r p l
        | None ->
          begin match maybe_extension extensions r previous lexemes with
            | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
      (Lessthan as t)
      ::(Word(tagname)
         ::(Space|Spaces _|Greaterthan|Greaterthans _ as aftertagname)
         ::tl as tlx) ->
      if StringSet.mem tagname inline_htmltags_set then
        main_loop_rev r [Word ""] lexemes
      else if not (blind_html || StringSet.mem tagname htmltags_set) then
        begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tlx
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
          loop [aftertagname;Word(tagname);Lessthan] 0 tl
        in
        let html, tl = read_html() in
        let html = Omd_lexer.string_of_tokens html in
        let attributes = Omd_utils.extract_html_attributes html in
        if List.mem ("media:type","text/omd") attributes then
          let innerHTML = Omd_utils.extract_inner_html html in
          let l_innerHTML = tag_setext main_loop (Omd_lexer.lex innerHTML) in
          let parsed_innerHTML = make_paragraphs(main_loop [] [] l_innerHTML) in
          let s_attributes =
            List.fold_left
              (fun r -> function
                 | "media:type","text/omd" -> r
                 | n, v ->
                   if String.contains v '"' then
                     Printf.sprintf "%s %s='%s'" r n v
                   else
                     Printf.sprintf "%s %s=\"%s\"" r n v
              )
              ""
              attributes
          in
          (* Here we have to delay the conversion to HTML because
             the inner markdown might use some references that are
             defined further in the document, hence the use of 
             the extension constructor [X]. *)
          let x = object
            val f = fun convert ->
              Printf.sprintf "<%s%s>%s</%s>"
                tagname
                s_attributes
                (convert parsed_innerHTML)
                tagname
            method name = "Html_block"
            method to_html ?indent to_html _t = Some(f to_html)
            method to_sexpr to_sexpr _t = Some(f to_sexpr)
            method to_t _t = Some([Html_block(f Omd_backend.html_of_md)])
          end
          in
          main_loop_rev (X(x) :: r) [Greaterthan] tl
        else            
          main_loop_rev (Html_block html :: r) [Greaterthan] tl

    (* inline html *)
    | _,
      (Lessthan as t)
      ::(Word(tagname) as w
                          ::((Space|Spaces _|Greaterthan|Greaterthans _)
                             ::_ as html_stuff) as tlx) ->
      if (strict_html && not(StringSet.mem tagname inline_htmltags_set))
      || not(blind_html || StringSet.mem tagname htmltags_set)
      then
        begin match maybe_extension extensions r previous lexemes with
          | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tlx
          | Some(r, p, l) -> main_loop_rev r p l
        end
      else
        let read_html() =
          let tag s = tag__md [Html s] in
          let rec loop accu n = function
            | Lessthan::Word("img"|"br"|"hr" as tn)::tl ->
              (* MAYBE self-closing tags *)
              begin
                let b, tl = read_until_gt ~bq:false tl in
                match List.rev b with
                | Slash::_ ->
                  let tv = Omd_lexer.string_of_tokens b in
                  loop (tag(sprintf "<%s%s>" tn tv) :: accu) n tl
                | _ ->
                  let tv = Omd_lexer.string_of_tokens b in
                  loop (tag(sprintf "<%s%s>" tn tv) :: accu)
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
              let b, tl = read_until_gt ~bq:false tl in
              let tv = Omd_lexer.string_of_tokens b in
              loop (tag(sprintf "<%s%s>" tn tv) :: accu)
                (if tn = tagname then n+1 else n)
                tl
            | x::tl ->
              loop (x::accu) n tl
            | [] ->
              List.rev accu, []
          in
          let b, tl = read_until_gt ~bq:false html_stuff in
          if (try ignore(read_until_lt ~bq:false b); false
              with Premature_ending -> true) then
            (* there must not be any '<' in b *)
            let tv = Omd_lexer.string_of_tokens b in
            loop [tag(Printf.sprintf "<%s%s>" tagname tv)] 0 tl
          else
            raise Premature_ending
        in
        (match try Some(read_html()) with Premature_ending -> None with
          | Some(html, tl) ->
            main_loop_rev (main_loop_rev [] [] html @ r) [Greaterthan] tl
          | None ->
            let text = Omd_lexer.string_of_token t in
            main_loop_rev (Text(text ^ tagname)::r) [w] html_stuff
        )
    (* / end of inline HTML. *)

    (* < : emails *)
    | _, (Lessthan as t)::tl ->
      begin match maybe_autoemail r previous lexemes with
        | Some(r,p,l) -> main_loop_rev r p l
        | None ->
          begin match maybe_extension extensions r previous lexemes with
            | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
              | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
           let b, tl = read_until_cparenth ~bq:true ~no_nl:false tl in
           (* new lines there are allowed *)
           let r (* updated result *) = match t with
             | Exclamations 0 -> Text "!" :: r
             | Exclamations n -> Text(String.make (n+1) '!') :: r
             | _ -> r in
           match
             try Some(read_until_space ~bq:false ~no_nl:true b)
             with Premature_ending -> None
           with
           | Some(url, tls) ->
             let title, should_be_empty_list =
               read_until_dq ~bq:true (snd (read_until_dq ~bq:true tls)) in
             let url = Omd_lexer.string_of_tokens url in
             let title = Omd_lexer.string_of_tokens title in
             main_loop_rev (Img("", url, title) :: r) [Cparenthesis] tl
           | None ->
             let url = Omd_lexer.string_of_tokens b in
             main_loop_rev (Img("", url, "") :: r) [Cparenthesis] tl
         end
       with
       | NL_exception ->
         begin match maybe_extension extensions r previous lexemes with
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
           | Some(r, p, l) -> main_loop_rev r p l
         end
      )

    (* img ref *)
    | _, (Exclamation as t)
         ::Obracket::Cbracket::Obracket::tl ->
      (* ref image insertion with no "alt" *)
      (* ![][ref] *)
      (try
         let id, tl = read_until_cbracket ~bq:true ~no_nl:true tl in
         let fallback = extract_fallback tl lexemes in
         let id = Omd_lexer.string_of_tokens id in
         main_loop_rev (Img_ref(rc, id, "", fallback) :: r) [Cbracket] tl
       with NL_exception ->
         begin match maybe_extension extensions r previous lexemes with
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
           | Some(r, p, l) -> main_loop_rev r p l
         end
      )


    (* img *)
    | _, (Exclamation|Exclamations _ as t)::Obracket::tl ->
      (* image insertion with "alt" *)
      (* ![Alt text](/path/to/img.jpg "Optional title") *)
      (try
         match read_until_cbracket ~bq:true tl with
         | alt, Oparenthesis::ntl ->
           (try
              let alt = Omd_lexer.string_of_tokens alt in
              let path_title, rest = 
                read_until_cparenth ~bq:true ~no_nl:false ntl in
              let path, title =
                try
                  read_until_space ~bq:true ~no_nl:true path_title
                with Premature_ending -> path_title, [] in
              let title, nothing =
                if title <> [] then
                  read_until_dq ~bq:true (snd(read_until_dq ~bq:true title))
                else [], [] in
              if nothing <> [] then
                raise NL_exception; (* caught right below *)
              let r =
                match t with
                | Exclamations 0 -> Text "!" :: r
                | Exclamations n -> Text(String.make (n+1) '!') :: r
                | _ -> r in
              let path = Omd_lexer.string_of_tokens path in
              let title = Omd_lexer.string_of_tokens title in
              main_loop_rev (Img(alt, path, title) :: r) [Cparenthesis] rest
            with
            | NL_exception
            (* if NL_exception was raised, then fall back to "text" *)
            | Premature_ending ->
              begin match maybe_extension extensions r previous lexemes with
                | None ->
                  main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
                | Some(r, p, l) ->
                  main_loop_rev r p l
              end
           )
         | alt, Obracket::Word(id)::Cbracket::ntl
         | alt, Obracket::(Space|Spaces _)::Word(id)::Cbracket::ntl
         | alt, Obracket::(Space|Spaces _)::Word(id)::(Space|Spaces _)
                ::Cbracket::ntl
         | alt, Obracket::Word(id)::(Space|Spaces _)::Cbracket::ntl ->
           let fallback = extract_fallback ntl lexemes in
           let alt = Omd_lexer.string_of_tokens alt in
           main_loop_rev (Img_ref(rc, id, alt, fallback)::r) [Cbracket] ntl
         | alt, Obracket::((Newline|Space|Spaces _|Word _|Number _)::_
                           as ntl) ->
           (try
              match read_until_cbracket ~bq:true ~no_nl:false ntl with
              | [], rest -> raise Premature_ending
              | id, rest ->
                let fallback = extract_fallback rest lexemes in
                let id = Omd_lexer.string_of_tokens id in
                let alt = Omd_lexer.string_of_tokens alt in
                main_loop_rev (Img_ref(rc, id, alt, fallback)::r)
                  [Cbracket]
                  rest
            with
            | Premature_ending
            | NL_exception ->
              begin match maybe_extension extensions r previous lexemes with
                | None ->
                  main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
                | Some(r, p, l) -> main_loop_rev r p l
              end
           )
         | _ ->
           begin match maybe_extension extensions r previous lexemes with
             | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
             | Some(r, p, l) -> main_loop_rev r p l
           end
       with
       | Premature_ending ->
         begin match maybe_extension extensions r previous lexemes with
           | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
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
        | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
      end
    | _, (Number _  as t):: tl ->
      begin match maybe_extension extensions r previous lexemes with
        | None -> main_loop_rev (Text(Omd_lexer.string_of_token t)::r) [t] tl
        | Some(r, p, l) -> main_loop_rev r p l
      end

    | _, (Ats _ | Bars _ | Carets _ | Cbraces _ | Cbrackets _ | Colons _
         | Commas _ | Cparenthesiss _ | Dollars _ | Dots _ | Doublequotes _
         | Equals _ | Exclamations _ | Greaterthans _ | Lessthans _
         | Minuss _ | Obraces _ | Obrackets _ | Oparenthesiss _
         | Percents _ | Pluss _ | Questions _ | Quotes _ | Semicolons _
         | Slashs _ | Stars _ | Tabs _ | Tildes _ | Underscores _ as tk)
         :: tl ->
      begin match maybe_extension extensions r previous lexemes with
        | None ->
          let tk0, tks = Omd_lexer.split_first tk in
          let text = Omd_lexer.string_of_token tk0 in
          main_loop_rev (Text text :: r) [tk0] (tks :: tl)
        | Some(r, p, l) ->
          main_loop_rev r p l
      end


  and main_loop (r:r) (previous:p) (lexemes:l) =
    assert_well_formed lexemes;
    List.rev (main_loop_rev r previous lexemes)

  let main_parse lexemes =
    main_loop [] [] (tag_setext main_loop lexemes)

  let parse lexemes =
    main_parse lexemes

end

let default_parse ?(extensions=[]) ?(default_lang="") lexemes =
  let e = extensions and d = default_lang in
  let module E = Default_env(Unit) in
  let module M =
    Make(struct
      include E
      let extensions = e
      let default_lang = d
    end)
  in
  M.main_parse lexemes



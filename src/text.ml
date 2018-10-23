(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013-2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Printf
open Utils
open Representation

type t =
  | Cat of t list
  | Text of string
  | Emph of t
  | Bold of t
  | Code of string
  | Br
  | NL
  | Url of href * t * title
  | Ref of name * string * fallback
  | Img_ref of name * alt * fallback
  | Html of string
  | Raw of string
  | Img of alt * src * title

and fallback = < to_string : string ; to_t : t >
and name = string
and alt = string
and src = string
and href = string
and title = string

module F = Format

let rec print ppf = function
  | Cat l ->
      F.pp_print_list ~pp_sep:F.pp_print_space print ppf l
  | Text s ->
      F.fprintf ppf "%S" s
  | Emph x ->
      F.fprintf ppf "@[<1>(emph@ %a)@]" print x
  | Bold x ->
      F.fprintf ppf "@[<1>(bold@ %a)@]" print x
  | Code x ->
      F.fprintf ppf "@[<1>(code@ %S)@]" x
  | Br ->
      F.pp_print_string ppf "br"
  | NL ->
      F.pp_print_string ppf "NL"
  | Url (url, _, _) ->
      F.fprintf ppf "@[<1>(url@ %S)@]" url
  | Ref (_, s, _) | Img_ref (_, s, _) ->
      F.fprintf ppf "@[<1>(ref@ %S)@]" s
  | Html html ->
      F.fprintf ppf "@[<1>(html@ %S)@]" html
  | Raw s ->
      F.fprintf ppf "%S" s
  | Img (_, src, _) ->
      F.fprintf ppf "@[<1>(img@ %S)@]" src

let rec html_of_md b md =
  let rec loop = function
    | Cat l ->
        List.iter loop l
    | Ref(_, _, fallback) | Img_ref (_, _, fallback) ->
        loop (fallback#to_t)
    | Img (alt, src, title) ->
        Buffer.add_string b "<img src=\"";
        Buffer.add_string b (htmlentities ~md:true src);
        Buffer.add_string b "\" alt=\"";
        Buffer.add_string b (htmlentities ~md:true alt);
        Buffer.add_string b "\" ";
        if title <> "" then begin
          Buffer.add_string b " title='";
          Buffer.add_string b (htmlentities ~md:true title);
          Buffer.add_string b "' "
        end;
        Buffer.add_string b "/>"
    | Text t ->
        (* Buffer.add_string b t; *)
        Buffer.add_string b (htmlentities ~md:true t)
    | Emph md ->
        Buffer.add_string b "<em>";
        loop md;
        Buffer.add_string b "</em>"
    | Bold md ->
        Buffer.add_string b "<strong>";
        loop md;
        Buffer.add_string b "</strong>"
    | Code c ->
        Buffer.add_string b "<code>";
        Buffer.add_string b (htmlentities ~md:false c);
        Buffer.add_string b "</code>"
    | Br ->
        Buffer.add_string b "<br/>"
    | Raw s ->
        Buffer.add_string b s
    | Html body ->
        Buffer.add_string b body
    | Url (href,s,title) ->
        Buffer.add_string b "<a href='";
        Buffer.add_string b (htmlentities ~md:true href);
        Buffer.add_string b "'";
        if title <> "" then begin
          Buffer.add_string b " title='";
          Buffer.add_string b (htmlentities ~md:true title);
          Buffer.add_string b "'"
        end;
        Buffer.add_string b ">";
        html_of_md b s;
        Buffer.add_string b "</a>"
    | NL ->
        Buffer.add_string b "\n"
  in
  loop md

let escape_markdown_characters s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '.' as c ->
        if i > 0 &&
           match s.[i-1] with
           | '0' .. '9' -> i+1 < String.length s && s.[i+1] = ' '
           | _ -> false
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '-' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && (s.[i+1] = ' '||s.[i+1] = '-'))
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '+' as c ->
        if (i = 0 || match s.[i-1] with ' '| '\n' -> true | _ -> false) &&
           (i+1 < String.length s && s.[i+1] = ' ')
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '!' as c ->
        if i+1 < String.length s && s.[i+1] = '[' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '<' as c ->
        if i <> String.length s - 1 &&
           (match s.[i+1] with 'a' .. 'z' | 'A' .. 'Z' -> false | _ -> true)
        then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '>' as c ->
        if i = 0 || (match s.[i-1] with ' ' | '\n' -> false | _ -> true) then
          Buffer.add_char b '\\';
        Buffer.add_char b c
    | '#' as c ->
        if i = 0 || s.[i-1] = '\n' then Buffer.add_char b '\\';
        Buffer.add_char b c
    | '\\' | '[' | ']' | '(' | ')' | '`' | '*' as c ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let rec markdown_of_md md =
  (* if debug then eprintf "(OMD) markdown_of_md(%S)\n%!" (sexpr_of_md md); *)
  let b = Buffer.create 64 in
  let rec loop = function
    | Cat l ->
        List.iter loop l
    | Ref (_, _, fallback) | Img_ref (_, _, fallback) ->
        loop (Raw (fallback#to_string))
    | Img (alt, src, title) ->
        Printf.bprintf b "![%s](%s \"%s\")" alt src title
    | Text t ->
        Printf.bprintf b "%s" (escape_markdown_characters t)
    | Emph md ->
        Buffer.add_string b "*";
        loop md;
        Buffer.add_string b "*"
    | Bold md ->
        Buffer.add_string b "**";
        loop md;
        Buffer.add_string b "**"
    | Code c ->
        let n = (* compute how many backquotes we need to use *)
          let filter (n:int) (s:int list) =
            if n > 0 && n < 10 then
              List.filter (fun e -> e <> n) s
            else
              s
          in
          let l = String.length c in
          let rec loop s x b i =
            if i = l then begin
              match filter b s with hd :: _ -> hd | [] -> x+1
            end else begin
              match c.[i] with
              | '`' ->
                  loop s x (succ b) (succ i)
              | _ ->
                  loop (filter b s) (max b x) 0 (succ i)
            end
          in
          loop [1;2;3;4;5;6;7;8;9;10] 0 0 0
        in
        Printf.bprintf b "%s" (String.make n '`');
        if c.[0] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" c;
        if c.[String.length c - 1] = '`' then Buffer.add_char b ' ';
        Printf.bprintf b "%s" (String.make n '`')
    | Br ->
        Buffer.add_string b "<br />"
    | Raw s ->
        Buffer.add_string b s
    | Html body ->
        Buffer.add_string b body
    | Url (href,s,title) ->
        if title = "" then
          bprintf b "[%s](%s)" (markdown_of_md s) href
        else
          bprintf b "[%s](%s \"%s\")" (markdown_of_md s) href title
    | NL ->
        if Buffer.length b = 1 ||
           (Buffer.length b > 1 &&
            not(Buffer.nth b (Buffer.length b - 1) = '\n' &&
                Buffer.nth b (Buffer.length b - 2) = '\n'))
        then
          Buffer.add_string b "\n"
  in
  loop md;
  let res = Buffer.contents b in
  (* if debug then eprintf "(OMD) markdown_of_md(%S) => %S\n%!" (sexpr_of_md md) res; *)
  res

module L = Lexer

(* N.B. Please do not use tabulations in your Markdown file! *)

module type Env = sig
  val default_lang: string
  val gh_uemph_or_bold_style: bool
  val blind_html: bool
  val strict_html: bool
  val warning: bool
  val warn_error: bool
end

module Unit = struct end

module Default_env (Unit : sig end) : Env = struct
  let default_lang = ""
  let gh_uemph_or_bold_style = true
  let blind_html = false
  let strict_html = false
  let warning = false
  let warn_error = false
end

module Make (Env:Env) =
struct
  include Env

  let warn = Utils.warn ~we:warn_error

  (* set of known HTML codes, extracted from:
     http://www.w3.org/TR/html4/charset.html, to be checked. *)
  let htmlcodes_set =
    StringSet.of_list
      [
        "AElig"; "Aacute"; "Acirc"; "Agrave"; "Alpha"; "Aring"; "Atilde";
        "Auml"; "Beta"; "Ccedil"; "Chi"; "Dagger"; "Delta"; "ETH"; "Eacute";
        "Ecirc"; "Egrave"; "Epsilon";  "Eta"; "Euml"; "Gamma"; "Iacute";
        "Icirc"; "Igrave"; "Iota"; "Iuml"; "Kappa"; "Lambda"; "Mu"; "Ntilde";
        "Nu"; "OElig"; "Oacute";  "Ocirc"; "Ograve"; "Omega"; "Omicron";
        "Oslash"; "Otilde"; "Ouml"; "Phi"; "Pi"; "Prime"; "Psi"; "Rho";
        "Scaron"; "Sigma";  "THORN"; "Tau"; "Theta";  "Uacute"; "Ucirc";
        "Ugrave"; "Upsilon"; "Uuml"; "Xi"; "Yacute"; "Yuml"; "Zeta"; "aacute";
        "acirc"; "acute"; "aelig"; "agrave"; "alefsym"; "alpha"; "amp"; "and";
        "ang"; "aring"; "asymp"; "atilde"; "auml"; "bdquo"; "beta"; "brvbar";
        "bull"; "cap"; "ccedil"; "cedil"; "cent"; "chi"; "circ"; "clubs";
        "cong"; "copy"; "crarr"; "cup"; "curren"; "dArr"; "dagger"; "darr";
        "deg"; "delta"; "diams";  "divide"; "eacute"; "ecirc"; "egrave";
        "empty"; "emsp"; "ensp"; "epsilon"; "equiv"; "eta"; "eth"; "euml";
        "euro"; "exist"; "fnof"; "forall"; "frac12"; "frac14"; "frac34";
        "frasl"; "gamma"; "ge"; "gt"; "hArr"; "harr"; "hearts"; "hellip";
        "iacute"; "icirc"; "iexcl"; "igrave"; "image"; "infin"; "int"; "iota";
        "iquest"; "isin"; "iuml"; "kappa"; "lArr"; "lambda"; "lang"; "laquo";
        "larr"; "lceil"; "ldquo"; "le"; "lfloor"; "lowast"; "loz"; "lrm";
        "lsaquo"; "lsquo"; "lt"; "macr"; "mdash"; "micro"; "middot"; "minus";
        "mu"; "nabla"; "nbsp"; "ndash"; "ne"; "ni"; "not"; "notin"; "nsub";
        "ntilde"; "nu";  "oacute"; "ocirc"; "oelig";  "ograve"; "oline";
        "omega"; "omicron"; "oplus"; "or"; "ordf"; "ordm"; "oslash"; "otilde";
        "otimes"; "ouml"; "para"; "part"; "permil"; "perp"; "phi"; "pi";
        "piv"; "plusmn"; "pound"; "prime"; "prod"; "prop"; "psi"; "quot";
        "rArr"; "radic"; "rang"; "raquo"; "rarr"; "rceil"; "rdquo"; "real";
        "reg"; "rfloor"; "rho"; "rlm"; "rsaquo"; "rsquo"; "sbquo"; "scaron";
        "sdot"; "sect"; "shy"; "sigma"; "sigmaf"; "sim"; "spades"; "sub";
        "sube"; "sum"; "sup"; "sup1"; "sup2"; "sup3"; "supe"; "szlig"; "tau";
        "there4"; "theta"; "thetasym"; "thinsp"; "thorn"; "tilde"; "times";
        "trade"; "uArr"; "uacute"; "uarr"; "ucirc"; "ugrave"; "uml"; "upsih";
        "upsilon"; "uuml"; "weierp"; "xi"; "yacute"; "yen"; "yuml"; "zeta";
        "zwj"; "zwnj"
      ]

  (* set of known inline HTML tags, from
     https://developer.mozilla.org/en-US/docs/HTML/Inline_elements *)
  let inline_htmltags_set =
    StringSet.of_list
      [
        "b"; "big"; "i"; "small"; "tt";
        "abbr"; "acronym"; "cite"; "code"; "dfn"; "em"; "kbd"; "strong"; "samp"; "var";
        "a"; "bdo"; "br"; "img"; "map"; "object"; "q"; "span"; "sub"; "sup";
        "button"; "input"; "label"; "select"; "textarea"
      ]

  (* N.B. it seems that there is no clear distinction between inline tags and
     block-level tags: in HTML4 it was not clear, in HTML5 it's even more
     complicated. So, the choice *here* is to specify a set of tags considered
     as "inline", cf. [inline_htmltags_set].  So there will be inline tags,
     non-inline tags, and unknown tags.*)

  (* set of HTML tags that may appear out of a body *)
  let notinbodytags =
    StringSet.of_list
      [
        "title";
        "link";
        "meta";
        "style";
        "html";
        "head";
        "body";
      ]

  (* All known HTML tags *)
  let htmltags_set =
    let tags =
      [
        "a"; "abbr"; "acronym"; "address"; "applet"; "area"; "article"; "aside";
        "audio"; "b"; "base"; "basefont"; "bdi"; "bdo"; "big"; "blockquote";
        "br"; "button"; "canvas"; "caption"; "center"; "cite"; "code"; "col";
        "colgroup"; "command"; "datalist"; "dd"; "del"; "details"; "dfn";
        "dialog"; "dir"; "div"; "dl"; "dt"; "em"; "embed"; "fieldset";
        "figcaption"; "figure"; "font"; "footer"; "form"; "frame"; "frameset";
        "h2"; "h3"; "h4"; "h5"; "h6";
        "h1"; "header"; "hr"; "i"; "iframe"; "img"; "input"; "ins"; "kbd";
        "keygen"; "label"; "legend"; "li"; "map"; "mark"; "menu"; "meter"; "nav";
        "noframes"; "noscript"; "object"; "ol"; "optgroup"; "option"; "output";
        "p"; "param"; "pre"; "progress"; "q"; "rp"; "rt"; "ruby"; "s"; "samp";
        "script"; "section"; "select"; "small"; "source"; "span"; "strike";
        "strong"; "style"; "sub"; "summary"; "sup"; "table"; "tbody"; "td";
        "textarea"; "tfoot"; "th"; "thead"; "time"; "tr"; "track"; "tt"; "u";
        "ul"; "var"; "video"; "wbr"
      ]
    in
    StringSet.union notinbodytags (StringSet.union inline_htmltags_set (StringSet.of_list tags))

  (* This functions fixes bad lexing trees, which may be built when
     extraction a portion of another lexing tree. *)
  let fix l =
    let rec loop accu = function
      | Delim (n, a) :: Delim (m, b) :: tl when a = b ->
          (* if trackfix then eprintf "(OMD) Ampersand 1\n"; *)
          loop accu (Delim (n+m, a) :: tl)
      | x :: tl ->
          loop (x :: accu) tl
      | [] ->
          List.rev accu
    in
    loop [] l

  (* [assert_well_formed] is a developer's function that helps to
     track badly constructed token lists.  This function has an
     effect only if [trackfix] is [true].  *)
  let assert_well_formed (l:tok list) : unit =
    if trackfix then
      let rec equiv l1 l2 = match l1, l2 with
        | [], [] ->
            true
        | Tag _ :: tl1, Tag _ :: tl2 ->
            equiv tl1 tl2
        | e1 :: tl1, e2 :: tl2 ->
            e1 = e2 && equiv tl1 tl2
        | _ ->
            false
      in
      assert (equiv (fix l) l)

  (* Generate fallback for references. *)
  let extract_fallback remains l =
    if debug then eprintf "(OMD) Omd_parser.extract_fallback\n%!";
    let rec loop accu = function
      | [] ->
          List.rev accu
      | e :: tl as r ->
          if r == remains then
            List.rev accu
          else begin
            match e, remains with
            | Delim (n, Cbracket), Delim (m, Cbracket) :: r when m + 1 = n && tl = r ->
                let accu = Word "]" :: accu in
                List.rev accu
            | _ ->
                loop (e :: accu) tl
          end
    in
    let a = loop [] l in
    object
      method to_string = L.string_of_tokens a
      method to_t = Text (L.string_of_tokens a)
    end

  let rec is_blank = function
    | Delim (_, (Space | Newline)) :: tl ->
        is_blank tl
    | [] ->
        true
    | _ ->
        false

  let delim n d l = if n = 0 then l else Delim (n, d) :: l

  let semph_or_bold n l =
    (* FIXME: use rpl call/return convention *)
    assert_well_formed l;
    assert (n > 0 && n < 4);
    match
      fsplit
        ~excl:(function Delim (_, Newline) :: _ -> true | _ -> false)
        ~f:(function
            | (Delim (b, Backslash) as x) :: (Delim (n, Star) as s) :: tl ->
                if b mod 2 = 0 then
                  Continue_with ([x], s :: tl)
                else
                  Continue_with ([Delim (1, Star); x], delim (n-1) Star tl)
            | (Delim (_, Space) as x) :: (Delim (_, Star) as s) :: tl ->
                Continue_with ([s; x], tl)
            | (Delim (_, Star) as s) :: tl ->
                if L.length s = n then
                  Split ([], tl)
                else
                  Continue
            | _ ->
                Continue
          ) l
    with
    | None ->
        None
    | Some (left,right) ->
        if is_blank left then None else Some (left,right)

  let sm_uemph_or_bold n l =
    assert_well_formed l;
    (* FIXME: use rpl call/return convention *)
    assert (n>0 && n<4);
    match
      fsplit
        ~excl:(function Delim (_, Newline) :: _ -> true | _ -> false)
        ~f:(function
            | (Delim (b, Backslash) as x) :: (Delim (n, Underscore) as s) :: tl ->
                if b mod 2 = 0 then
                  Continue_with ([x], s :: tl)
                else
                  Continue_with ([Delim (1, Underscore); x], delim (n-1) Underscore tl)
            | (Delim (_, Space) as x) :: (Delim (_, Underscore) as s)::tl ->
                Continue_with ([s; x], tl)
            | (Delim (_, Underscore) as s) :: tl ->
                if L.length s = n then
                  Split([],tl)
                else
                  Continue
            | _ ->
                Continue
          )
        l
    with
    | None ->
        None
    | Some (left,right) ->
        if is_blank left then None else Some (left, right)

  let gh_uemph_or_bold n l =
    assert_well_formed l;
    (* FIXME: use rpl call/return convention *)
    assert (n>0 && n<4);
    match
      fsplit
        ~excl:(function Delim (_, Newline) :: _ -> true | _ -> false)
        ~f:(function
            | (Delim (b, Backslash) as x) :: (Delim (n, Underscore) as s) :: tl ->
                if b mod 2 = 0 then
                  Continue_with ([x], s :: tl)
                else
                  Continue_with ([Delim (1, Underscore); x], delim (n-1) Underscore tl)
            | (Delim (_, Space) as x) :: (Delim (_, Underscore) as s) :: tl ->
                Continue_with ([s; x], tl)
            | (Delim (_, Underscore) as s) :: (Word _ | Number _ as w) :: tl ->
                Continue_with ([w; s], tl)
            | (Delim (_, Underscore) as s) :: tl ->
                if L.length s = n then
                  Split ([],tl)
                else
                  Continue
            | _ ->
                Continue
          ) l
    with
    | None ->
        None
    | Some (left,right) ->
        if is_blank left then None else Some (left,right)

  let uemph_or_bold n l =
    assert_well_formed l;
    (* FIXME: use rpl call/return convention *)
    if gh_uemph_or_bold_style then
      gh_uemph_or_bold n l
    else
      sm_uemph_or_bold n l

  let eat_blank =
    eat (function Delim (_, (Space | Newline)) -> true| _ -> false)

  (* [bcode] parses code that's delimited by backquote(s) *)
  let bcode r _p l =
    assert_well_formed l;
    let e, tl =
      match l with
      | Delim (_, Backquote) as e :: tl ->
          e, tl
      | _ ->
          failwith "Omd_parser.bcode is wrongly called"
    in
    let rec code_block accu = function
      | [] ->
          None
      | Delim (_, Backquote) as b :: tl ->
          if e = b then
            match accu with
            | Delim (1, Newline) :: accu ->
                Some (List.rev accu, tl)
            | _ ->
                Some (List.rev accu, tl)
          else
            code_block (b::accu) tl
      | Tag (_, _) :: tl ->
          code_block accu tl
      | e :: tl ->
          code_block (e::accu) tl
    in
    match code_block [] tl with
    | None ->
        None
    | Some (cb, l) ->
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
        let code = L.string_of_tokens cb in
        if debug then
          eprintf "(OMD) clean_bcode %S => %S\n%!" code (clean_bcode code);
        Some (Code (clean_bcode code) :: r, [Delim (1, Backquote)], l)

  exception NL_exception
  exception Premature_ending

  let read_until ldelim rdelim ?(bq = false) ?(no_nl = false) l =
    assert_well_formed l;
    let rec loop accu n = function
      | Delim (1, Backslash) :: Delim (x, b) :: tl when b = ldelim ->
          loop (Delim (1, b) :: accu) n (delim (x-1) b tl)
      | Delim (1, Backslash) :: Delim (x, b) :: tl when Some b = rdelim ->
          loop (Delim (1, b) :: accu) n (delim (x-1) b tl)
      | Delim (1, Backslash) as e :: tl ->
          loop (e :: accu) n tl
      | Delim (x, Backslash) :: tl ->
          loop (delim (x/2) Backslash accu) n (delim (x mod 2) Backslash tl)
      | Delim (_, Backquote) as e :: tl as l ->
          if bq then
            match bcode [] [] l with
            | None ->
                loop (e :: accu) n tl
            | Some (r, _, tl) ->
                loop (* not very pretty kind of hack *)
                  (List.rev (L.lex (markdown_of_md (Cat r)))@accu)
                  n
                  tl
          else
            loop (e :: accu) n tl
      | Delim (x, b) as e :: tl when Some b = rdelim ->
          loop (e :: accu) (n+x) tl
      | Delim (x, b) :: tl when b = ldelim ->
          if n = 0 then
            List.rev accu, delim (x-1) b tl
          else
            loop (match accu with Delim (x, b) :: accu when b = ldelim -> Delim (x+1, b) :: accu | _ -> Delim (1, b) :: accu)
              (n-1) (delim (x-1) b tl)
      | Delim (_, Newline) as e :: tl ->
          if no_nl then
            raise NL_exception
          else
            loop (e :: accu) n tl
      | e :: tl ->
          loop (e :: accu) n tl
      | [] ->
          raise Premature_ending
    in
    if debug then
      eprintf "Omd_parser.read_until %S bq=%b no_nl=%b\n%!" (L.string_of_tokens l) bq no_nl;
    let res = loop [] 0 l in
    if debug then
      eprintf "Omd_parser.read_until %S bq=%b no_nl=%b => %S\n%!" (L.string_of_tokens l) bq no_nl (L.string_of_tokens (fst res));
    res

  let read_until_cparenth ?bq ?no_nl l =
    read_until Cparenthesis (Some Oparenthesis) ?bq ?no_nl l

  let read_until_dq ?bq ?no_nl l =
    read_until Doublequote None ?bq ?no_nl l

  let read_until_obracket ?bq ?no_nl l =
    read_until Obracket None ?bq ?no_nl l

  let read_until_cbracket ?bq ?no_nl l =
    read_until Cbracket (Some Obracket) ?bq ?no_nl l

  let read_until_space ?bq ?no_nl l =
    read_until Space None ?bq ?no_nl l

  (* maybe a reference *)
  let maybe_reference r _p l =
    assert_well_formed l;
    (* this function is called when we know it's not a link although
       it started with a '[' *)
    (* So it could be a reference or a link definition. *)
    let maybe_ref l =
      let text, remains = read_until_cbracket ~bq:true l in
      (* check that there is no ill-placed open bracket *)
      if (try ignore(read_until_obracket ~bq:true text); true
          with Premature_ending -> false) then
        raise Premature_ending; (* <-- ill-placed open bracket *)
      let blank, remains = read_until_obracket ~bq:true remains in
      (* check that there are no unwanted characters between CB and OB. *)
      if eat (let flag = ref true in
              function (* allow only a space, multiple spaces, or a newline *)
              | Delim (1, Newline) -> !flag && (flag := false; true)
              | Delim (_, Space) -> !flag && (flag := false; true)
              | _ -> false) blank <> [] then
        raise Premature_ending (* <-- not a regular reference *)
      else begin
        match read_until_cbracket ~bq:true remains with
        | [], remains ->
            let fallback = extract_fallback remains (Delim (1, Obracket) :: l) in
            let id = L.string_of_tokens text in (* implicit anchor *)
            Some (Ref (id, id, fallback) :: r, [Delim (1, Cbracket)], remains)
        | id, remains ->
            let fallback = extract_fallback remains (Delim (1, Obracket) :: l) in
            Some(Ref (L.string_of_tokens id, L.string_of_tokens text, fallback) :: r, [Delim (1, Cbracket)], remains)
      end
    in
    let maybe_nonregular_ref l =
      let text, remains = read_until_cbracket ~bq:true l in
      (* check that there is no ill-placed open bracket *)
      if (match read_until_obracket ~bq:true text with _ -> true | exception Premature_ending -> false) then
        raise Premature_ending; (* <-- ill-placed open bracket *)
      let fallback = extract_fallback remains (Delim (1, Obracket) :: l) in
      let id = L.string_of_tokens text in (* implicit anchor *)
      Some (Ref (id, id, fallback) :: r, [Delim (1, Cbracket)], remains)
    in
    try
      maybe_ref l
    with Premature_ending | NL_exception ->
    try
      maybe_nonregular_ref l
    with Premature_ending | NL_exception ->
      None

  (* maybe a link *)
  let maybe_link main_loop r _p l =
    if debug then eprintf "(OMD) # maybe_link\n";
    assert_well_formed l;
    let read_url name l =
      if debug then
        eprintf "(OMD) # maybe_link>read_url %S\n" (L.string_of_tokens l);
      try
        let l_cp, r_cp = read_until_cparenth ~no_nl:true ~bq:false l in
        if debug then
          eprintf "(OMD) maybe_link >> l_cp=%S r_cp=%S\n%!"
            (L.string_of_tokens l_cp) (L.string_of_tokens r_cp);
        try
          let l_dq, r_dq = read_until_dq ~no_nl:true ~bq:false l in
          if debug then
            eprintf "(OMD) maybe_link >> l_dq=%S r_dq=%S\n%!"
              (L.string_of_tokens l_dq) (L.string_of_tokens r_dq);
          (* maybe title *)
          if List.length l_cp > List.length l_dq then begin (* title *)
            if debug then eprintf "(OMD) maybe_link >> title\n%!";
            let url =
              match List.rev l_dq with
              | (Delim (1, Newline) | Delim (_, Space)) :: (Delim (1, Newline) | Delim (_, Space)) :: tl
              | (Delim (1, Newline) | Delim (_, Space)) :: tl ->
                  L.string_of_tokens (List.rev tl)
              | _ ->
                  L.string_of_tokens l_dq
            in
            let title, rest = read_until_dq ~no_nl:false ~bq:false r_dq in
            let rest = snd(read_until_cparenth rest) in
            let title = L.string_of_tokens title in
            Some (Url (url, name, title) :: r, [Delim (1, Cparenthesis)], rest)
          end else (* no title *)
            raise Premature_ending
        with NL_exception | Premature_ending -> (* no title *)
          if debug then eprintf "(OMD) maybe_link >> no title\n%!";
          let url =
            match List.rev l_cp with
            | (Delim (1, Newline) | Delim (_, Space)) :: (Delim (1, Newline) | Delim (_, Space)) :: tl
            | (Delim (1, Newline) | Delim (_, Space)) :: tl ->
                List.rev tl
            | _ ->
                l_cp
          in
          let title, rest = [], r_cp in
          let url = L.string_of_tokens url in
          let title = L.string_of_tokens title in
          Some (Url(url, name, title) :: r, [Delim (1, Cparenthesis)], rest)
      with NL_exception | Premature_ending ->
        None
    in
    let read_name l =
      (* it's not really the "name" of a URL but what
         corresponds to the inner HTML of an HTML 'A' tag *)
      if debug then eprintf "(OMD) # maybe_link> read_name\n";
      try
        match read_until_cbracket ~bq:true l with
        | name, Delim (1, Oparenthesis) :: tl ->
            read_url (main_loop [] [Delim (1, Obracket)] name) (eat_blank tl)
        | name, Delim (n, Oparenthesis) :: tl ->
            read_url (main_loop [] [Delim (1, Obracket)] name) (delim (n-1) Oparenthesis tl)
        | _ ->
            None
      with Premature_ending | NL_exception ->
        None
    in
    read_name l

  (* Returns [(r,p,l)] where [r] is the result, [p] is the last thing
     read, and [l] is what remains. *)
  let spaces_at_beginning_of_line n r previous lexemes =
    assert_well_formed lexemes;
    assert (n > 0);
    if n <= 3 then begin
      match lexemes with
      | [] | Delim (_, Newline) :: _  -> (* blank line, skip spaces *)
          r, previous, lexemes
      | _ :: _ ->
          Text (" ") :: r, previous, lexemes
    end else begin (* n>=4, blank line or indented code *)
      match lexemes with
      | [] | Delim (_, Newline) :: _  ->
          r, previous, lexemes
      | _ ->
          if debug then eprintf "(OMD) Omd_parser.icode or Omd_parser.main_loop is broken\n%!";
          assert false
    end

  let spaces_not_at_beginning_of_line ?(html = false) n r lexemes =
    assert_well_formed lexemes;
    assert (n > 0);
    if n = 1 then
      Text " " :: r, [Delim (1, Space)], lexemes
    else begin
      match lexemes with
      | Delim (1, Newline) :: tl when not html ->
          if debug then
            eprintf "(OMD) 2 or more spaces before a newline, eat the newline\n%!";
          Br :: r, [Delim (n, Space)], tl
      | Delim (k, Newline) :: tl when not html && k >= 2 ->
          if debug then
            eprintf "(OMD) 2 or more spaces before a newline, eat 1 newline";
          let newlines = Delim (k-1, Newline) in
          Br :: r, [Delim (n, Space)], newlines :: tl
      | _ ->
          assert (n > 1);
          Text (String.make n ' ') :: r, [Delim (n, Space)], lexemes
    end

  let maybe_autoemail r _p l =
    assert_well_formed l;
    match l with
    | Delim (1, Lessthan) :: tl ->
        begin match
          fsplit ~excl:(function
              | Delim (_, (Newline|Space)) :: _ | [] ->
                  true
              | _ ->
                  false
            )
            ~f:(function Delim (1, At) :: tl -> Split ([], tl) | _ -> Continue)
            tl
        with
        | None ->
            None
        | Some (left, right) ->
            match
              fsplit
                ~excl:(function
                    | Delim (_, (Newline|Space)) :: _ | [] -> true
                    | _ -> false
                  )
                ~f:(function
                    | Delim (n, Greaterthan) :: tl -> Split ([], delim (n-1) Greaterthan tl)
                    | _ -> Continue
                  ) right
            with
            | None ->
                None
            | Some (domain, tl) ->
                let email = L.string_of_tokens left ^ "@" ^ L.string_of_tokens domain in
                Some (Url ("mailto:" ^ email, Text email, "") :: r, [Delim (1, Greaterthan)], tl)
        end
    | _ ->
        failwith "Omd_parser.maybe_autoemail: wrong use of the function."

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

  let mediatypetextomd : string list ref = ref []

  exception Orphan_closing of string * tok list * tok list

  let rec main_impl_rev ~html (r : t list) previous lexemes =
    (* if debug then eprintf "(OMD) main_impl_rev html=%b\n%!" html; *)
    assert_well_formed lexemes;
    (* if debug then *)
    (*   eprintf "(OMD) main_impl_rev html=%b r=%s p=(%s) l=(%s)\n%!" *)
    (*     html *)
    (*     (Backend.sexpr_of_md (List.rev r)) *)
    (*     (L.destring_of_tokens previous) *)
    (*     (L.destring_of_tokens lexemes); *)
    match previous, lexemes with
    (* no more to process *)
    | _, [] ->
        (* return the result (/!\ it has to be reversed as some point) *)
        r

    (* Tag: tag system $\cup$ high-priority extension mechanism *)
    | _, Tag _ :: _ ->
        assert false

    (* minus *)
    | ([] | [Delim (_, Newline)]), (Delim (_, Minus) as t) :: (Delim (_, Space) :: _ as tl) ->
        main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
    | ([] | [Delim (_, Newline)]), (Delim (_, Minus) as t) :: tl ->
        main_impl_rev ~html (Text (L.string_of_token t)::r) [t] tl

    | _, (Delim (_, Hash) as t) :: tl -> (* hash -- no title *)
        main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl

    (* spaces after a newline: could lead to hr *)
    | ([] | [Delim (_, Newline)]), (Delim (_, Space) as sp) :: tl ->
        (* No [Hr], but maybe [Ul], [Ol], code,... *)
        let n = L.length sp in
        let r, p, l =
          spaces_at_beginning_of_line n r previous tl
        in
        main_impl_rev ~html r p l

    (* spaces anywhere *)
    | _, (Delim (_, Space) as t) :: tl ->
        (* too many cases to be handled here *)
        let n = L.length t in
        let r, p, l = spaces_not_at_beginning_of_line ~html n r tl in
        main_impl_rev ~html r p l

    (* underscores *)
    | _, (Delim (1, Underscore) as t) :: tl -> (* one "orphan" underscore, or emph *)
        begin match uemph_or_bold 1 tl with
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        | Some (x, new_tl) ->
            main_impl_rev ~html (Emph (main_impl ~html [] [t] x) :: r) [t] new_tl
        end
    | _, (Delim ((2|3) as n, Underscore) as t) :: tl ->
        (* 2 or 3 "orphan" underscores, or emph/bold *)
        begin match uemph_or_bold n tl with
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        | Some (x, new_tl) ->
            if n = 2 then (* 1 underscore *)
              main_impl_rev ~html (Bold (main_impl ~html [] [t] x) :: r) [t] new_tl
            else (* 2 underscores *)
              main_impl_rev ~html (Emph (Bold (main_impl ~html [] [t] x)) :: r) [t] new_tl
        end

    | ([] | [Delim (_, Newline)]), (Delim (1, Star) as t) :: tl -> (* maybe hr *)
        begin match semph_or_bold 1 tl with
        | Some(x, new_tl) ->
            main_impl_rev ~html (Emph (main_impl ~html [] [t] x) :: r) [t] new_tl
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end
    | _, (Delim (1, Star) as t) :: tl -> (* one "orphan" star, or emph // can't be hr *)
        begin match semph_or_bold 1 tl with
        | Some(x, new_tl) ->
            main_impl_rev ~html (Emph (main_impl ~html [] [t] x) :: r) [t] new_tl
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end
    | _, (Delim ((2|3) as n, Star) as t) :: tl ->
        (* 2 or 3 "orphan" stars, or emph/bold *)
        begin match semph_or_bold n tl with
        | Some (x, new_tl) ->
            if n = 2 then
              main_impl_rev ~html (Bold (main_impl ~html [] [t] x) :: r) [t] new_tl
            else
              main_impl_rev ~html (Emph (Bold (main_impl ~html [] [t] x)) :: r) [t] new_tl
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end

    (* backslashes *)
    | _, Delim (1, Backslash) :: Delim (n, Newline) :: tl ->
        assert (n >= 0); (* \\n\n\n\n... *)
        main_impl_rev ~html (Br :: r) [Delim (1, Backslash); Delim (1, Newline)] (delim (n-1) Newline tl)
    | _, Delim (1, Backslash) :: Delim (n, (Backquote|Star|Underscore|Obrace|Cbrace|Obracket|Cbracket|Oparenthesis|Cparenthesis|Plus|Minus|Dot|Exclamation|Hash|Greaterthan|Lessthan as d)) :: tl ->
        assert (n >= 0);
        main_impl_rev ~html (Text (String.make 1 (L.char_of_delim d)) :: r) [Delim (1, Backslash); Delim (1, d)] (delim (n-1) d tl)

    | _, (Delim (1, Backslash) as t) :: tl ->
        main_impl_rev ~html (Text "\\" :: r) [t] tl
    | _, (Delim (n, Backslash) as t) :: tl (* when n >= 2 *) -> (* \\\\... *)
        main_impl_rev ~html (Text (String.make (n/2) '\\') :: r) [t] (delim (n mod 2) Backslash tl)

    (* < *)
    | _, (Delim (_, Lessthan) as t) :: (Word ("http"|"https"|"ftp"|"ftps"|"ssh"|"afp"|"imap") as w) ::
         Delim (1, Colon) :: Delim (n, Slash) :: tl when n >= 2 ->
        (* "semi-automatic" URLs *)
        let rec read_url accu = function
          | Delim (_, Newline) :: _ ->
              None
          | Delim (1, Greaterthan) :: tl ->
              let url =
                L.string_of_token w ^ "://" ^
                (if n = 2 then "" else String.make (n-3) '/') ^ L.string_of_tokens (List.rev accu)
              in
              Some (url, tl)
          | x :: tl ->
              read_url (x :: accu) tl
          | [] ->
              None
        in
        begin match read_url [] tl with
        | Some (url, new_tl) ->
            let r =
              match t with
              | Delim (n, Lessthan) when n >= 2 ->
                  Text(String.make (n-1) '<') :: r
              | _ ->
                  r
            in
            main_impl_rev ~html (Url (url, Text url,"") :: r) [] new_tl
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end

    (* Word(w) *)
    | _, Word w :: tl ->
        main_impl_rev ~html (Text w :: r) [Word w] tl

    (* newline at the end *)
    | _, [Delim (1, Newline)] ->
        NL :: r

    (* named html entity *)
    | _, Delim (1, Ampersand) :: ((Word w :: (Delim (n, Semicolon) as s) :: tl) as tl2) ->
        if StringSet.mem w htmlcodes_set then
          main_impl_rev ~html (Raw ("&"^w^";") :: r) [s] (delim (n-1) Semicolon tl)
        else
          main_impl_rev ~html (Raw ("&amp;") :: r) [] tl2

    (* digit-coded html entity *)
    | _, Delim (1, Ampersand) :: ((Delim (1, Hash) :: Number w :: (Delim (n, Semicolon) as s) :: tl) as tl2) ->
        if String.length w <= 4 then
          main_impl_rev ~html (Raw ("&#"^w^";")::r) [s] (delim (n-1) Semicolon tl)
        else
          main_impl_rev ~html (Raw ("&amp;") :: r) [] tl2

    (* maybe hex digit-coded html entity *)
    | _, Delim (1, Ampersand) :: ((Delim (1, Hash) :: Word w :: (Delim (n, Semicolon) as s) :: tl) as tl2) when is_hex w ->
        if String.length w <= 4 then
          main_impl_rev ~html (Raw ("&#"^w^";") :: r) [s] (delim (n-1) Semicolon tl)
        else
          main_impl_rev ~html (Raw ("&amp;") :: r) [] tl2

    (* Several Ampersands (more than 2) *)
    | _, Delim (n, Ampersand) :: tl ->
        main_impl_rev ~html (Raw ("&amp;") :: r) [] (delim (n-1) Ampersand tl)

    (* backquotes *)
    | _, (Delim (_, Backquote) as t) :: tl ->
        begin match bcode r previous lexemes with
        | Some(r, p, l) -> main_impl_rev ~html r p l
        | None -> main_impl_rev ~html (Text (L.string_of_token t)::r) [t] tl
        end

    (* HTML *)

    (* awaited orphan html closing tag *)
    | _, Delim (1, Lessthan) :: Delim (1, Slash) :: Word w :: Delim (n, Greaterthan) :: tl when !mediatypetextomd <> [] ->
        raise (Orphan_closing(w, lexemes, delim (n-1) Greaterthan tl))

    (* inline HTML *)
    | _, (Delim (1, Lessthan) as t) :: ((Word tagnametop as w) :: (Delim (_, (Space|Greaterthan)) :: _ as html_stuff) as tlx) ->
        if (strict_html && not(StringSet.mem tagnametop inline_htmltags_set)) ||
           not (blind_html || StringSet.mem tagnametop htmltags_set)
        then
          main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tlx
        else
          let read_html() =
            let module T = struct
              type text = t

              type t =
                | Awaiting of string
                | Open of string

              type interm =
                | HTML of string * (string * string option) list * interm list
                | TOKENS of L.t
                | MD of text

              let rec md_of_interm_list = function
                | [] ->
                    []
                | HTML _ :: tl ->
                    Html "TODO" :: md_of_interm_list tl
                | MD md :: tl ->
                    md :: md_of_interm_list tl
                | TOKENS t1 :: TOKENS t2 :: tl ->
                    md_of_interm_list (TOKENS (t1@t2) :: tl)
                | TOKENS t :: tl ->
                    main_impl ~html [] [Word ""] t :: md_of_interm_list tl

              let string_of_tagstatus tagstatus =
                let b = Buffer.create 64 in
                List.iter (function
                    | Open t ->
                        bprintf b "{I/Open %s}" t
                    | Awaiting t ->
                        bprintf b "{I/Awaiting %s}" t
                  ) tagstatus;
                Buffer.contents b
            end
            in
            let add_token_to_body x body = T.TOKENS[x]::body in
            let rec loop (body:T.interm list) attrs tagstatus tokens =
              if debug then
                eprintf "(OMD) 3718 loop tagstatus=(%s) %s\n%!"
                  (* eprintf "(OMD) 3718 loop tagstatus=(%s) body=(%s) %s\n%!" *)
                  (T.string_of_tagstatus tagstatus)
                  (* (Omd_backend.sexpr_of_md(T.md_of_interm_list body)) *)
                  (L.destring_of_tokens tokens);
              match tokens with
              | [] ->
                  begin match tagstatus with
                  | [] ->
                      Some (body, tokens)
                  | T.Open(t)::_ when StringSet.mem t html_void_elements ->
                      Some (body, tokens)
                  | _ ->
                      if debug then eprintf "(OMD) Not enough to read for inline HTML\n%!";
                      None
                  end
              | Delim (n, Lessthan) :: tokens when n >= 2 ->
                  begin match tagstatus with
                  | T.Awaiting _ :: _ ->
                      None
                  | _ ->
                      loop (add_token_to_body (Delim (n-1, Lessthan)) body)
                        attrs tagstatus (Delim (1, Lessthan) :: tokens)
                  end
              (* self-closing tags *)
              | Delim (1, Slash) :: Delim (1, Greaterthan) :: tokens ->
                  begin match tagstatus with
                  | T.Awaiting tagname :: tagstatus when StringSet.mem tagname html_void_elements ->
                      loop [T.HTML (tagname, attrs, [])] [] tagstatus tokens
                  | _ ->
                      loop (T.TOKENS [Delim (1, Greaterthan); Delim (1, Slash)]::body)
                        attrs tagstatus tokens
                  end
              (* multiple newlines are not to be seen in inline HTML *)
              | Delim (n, Newline) :: _ when n >= 2 ->
                  if debug then eprintf "(OMD) Multiple lines in inline HTML\n%!";
                  begin match tagstatus with
                  | [] ->
                      Some (body, tokens)
                  | _ ->
                      warn "multiple newlines in inline HTML";
                      None
                  end
              (* maybe code *)
              | Delim (_, Backquote) as b :: tl ->
                  begin match tagstatus with
                  | T.Awaiting _ :: _ ->
                      if debug then eprintf "(OMD) maybe code in inline HTML: no code\n%!";
                      None
                  | [] ->
                      if debug then eprintf "(OMD) maybe code in inline HTML: none\n%!";
                      None
                  | T.Open _ :: _ ->
                      if debug then eprintf "(OMD) maybe code in inline HTML: let's try\n%!";
                      begin match bcode [] [Delim (1, Space)] tokens with
                      | Some (Code _ :: _ as c, _, l) ->
                          if debug then eprintf "(OMD) maybe code in inline HTML: confirmed\n%!";
                          loop (T.MD (Cat c) :: body) [] tagstatus l
                      | _ ->
                          if debug then eprintf "(OMD) maybe code in inline HTML: failed\n%!";
                          loop (T.TOKENS [b] :: body) [] tagstatus tl
                      end
                  end
              (* closing the tag *)
              | Delim (1, Lessthan) :: Delim (1, Slash) :: (Word tagname as w) :: (Delim (n, Greaterthan) as g) :: tokens ->
                  begin match tagstatus with
                  | T.Open t :: _ when t = tagname ->
                      if debug then eprintf "(OMD) 4136 properly closing %S tokens=%s\n%!" t (L.string_of_tokens tokens);
                      Some (body, delim (n-1) Greaterthan tokens)
                  | T.Open t :: _ ->
                      if debug then eprintf "(OMD) 4144 wrongly closing %S with %S 1\n%!" t tagname;
                      loop (T.TOKENS [g; w; Delim (1, Slash); Delim (1, Lessthan)] :: body) [] tagstatus tokens
                  | T.Awaiting t :: _ ->
                      if debug then eprintf "(OMD) 4149 wrongly closing %S with %S 2\n%!" t tagname;
                      None
                  | [] ->
                      if debug then eprintf "(OMD) 4154 wrongly closing nothing with %S 3\n%!" tagname;
                      None
                  end
              (* tag *)
              | Delim (1, Lessthan) :: (Word tagname as word) :: tokens
                when blind_html ||
                     (strict_html && StringSet.mem tagname inline_htmltags_set) ||
                     (not strict_html && StringSet.mem tagname htmltags_set) ->
                  if debug then eprintf "(OMD) <%s...\n%!" tagname;
                  begin match tagstatus with
                  | T.Open(t) :: _ when t <> tagname && StringSet.mem t html_void_elements ->
                      None
                  | T.Awaiting _ :: _ ->
                      None
                  | _ ->
                      if debug then eprintf "(OMD) 3796 tag %s, attrs=[]\n%!" tagname;
                      begin match loop [] [] (T.Awaiting tagname :: tagstatus) tokens with
                      | None ->
                          loop (T.TOKENS [word; Delim (1, Lessthan)] :: body) attrs tagstatus tokens
                      | Some (b,tokens) ->
                          Some (b@body, tokens)
                      end
                  end
              (* end of opening tag *)
              | Delim (1, Greaterthan) :: tokens ->
                  if debug then
                    eprintf "(OMD) 4185 end of opening tag tokens=%s tagstatus=%s\n%!"
                      (L.string_of_tokens tokens) (T.string_of_tagstatus tagstatus);
                  begin match tagstatus with
                  | T.Awaiting t :: tagstatus as ts ->
                      begin match loop body [] (T.Open t :: tagstatus) tokens with
                      | None ->
                          if debug then eprintf "(OMD) 4186 Couldn't find an closing tag for %S\n%!" t;
                          None
                      | Some(b, tokens) ->
                          if debug then
                            eprintf "(OMD) 4192 Found a closing tag %s ts=%s \ tokens=%s\n%!" t
                              (T.string_of_tagstatus ts) (L.string_of_tokens tokens);
                          match tagstatus with
                          | [] ->
                              Some(T.HTML (t, attrs, b) :: body, tokens)
                          | _ ->
                              (* Note: we don't care about the value of
                                 [attrs] here because in we have a
                                 [tagstatus] matches [T.Open _ :: _] and
                                 there's a corresponding filter that will
                                 take care of attrs that will take care of
                                 it. *)
                              loop (T.HTML (t, attrs, b) :: body) [] tagstatus tokens
                      end
                  | T.Open _ :: _ ->
                      if debug then eprintf "(OMD) Turns out an `>` isn't for an opening tag\n%!";
                      loop (T.TOKENS [Delim (1, Greaterthan)] :: body) attrs tagstatus tokens
                  | [] ->
                      if debug then eprintf "(OMD) 4202 tagstatus=[]\n%!";
                      None
                  end

              (* maybe attribute *)
              | (Delim (_, (Colon|Underscore)) | Word _ as t) :: tokens
              | Delim (_, Space) :: (Delim (_, (Colon|Underscore)) | Word _ as t) :: tokens
                when (match tagstatus with T.Awaiting _ :: _ -> true | _ -> false) ->
                  let module Attribute_value = struct
                    type t = Empty of name | Named of name | Void
                    and name = string
                  end
                  in
                  let open Attribute_value in
                  let rec extract_attribute accu = function
                    | (Delim (_, Space) | Delim (1, Newline)) :: tokens->
                        Empty (L.string_of_tokens (List.rev accu)), tokens
                    | Delim (_, Greaterthan) :: _ as tokens->
                        Empty (L.string_of_tokens (List.rev accu)), tokens
                    | Delim (1, Equal) :: tokens ->
                        Named (L.string_of_tokens (List.rev accu)), tokens
                    | Delim (_, (Colon|Underscore|Minus|Dot)) | Word _ | Number _ as t :: tokens ->
                        extract_attribute (t :: accu) tokens
                    | tokens ->
                        Void, tokens
                  in
                  begin match extract_attribute [t] tokens with
                  | Empty attributename, tokens ->
                      (* attribute with no explicit value *)
                      loop body ((attributename, None) :: attrs) tagstatus tokens
                  | Named attributename, tokens ->
                      begin match tokens with
                      | Delim (2, Quote) :: tokens ->
                          if debug then eprintf "(OMD) (IHTML) empty attribute 1 %S\n%!" (L.string_of_tokens tokens);
                          loop body ((attributename, Some "") :: attrs) tagstatus tokens
                      | Delim (1, Quote) :: tokens ->
                          if debug then eprintf "(OMD) (IHTML) non empty attribute 1 %S\n%!" (L.string_of_tokens tokens);
                          begin match
                            fsplit
                              ~excl:(function
                                  | Delim (n, Quote) :: _ when n >= 2 ->
                                      true
                                  | _ ->
                                      false
                                )
                              ~f:(function
                                  | Delim (1, Quote) :: tl ->
                                      Split ([], tl)
                                  | _ ->
                                      Continue
                                ) tokens
                          with
                          | None -> None
                          | Some(at_val, tokens) ->
                              loop body ((attributename, Some (L.string_of_tokens at_val)) :: attrs) tagstatus tokens
                          end
                      | Delim (2, Doublequote) :: tokens ->
                          if debug then
                            eprintf "(OMD) (IHTML) empty attribute 2 %S\n%!"
                              (L.string_of_tokens tokens);
                          loop body ((attributename, Some "") :: attrs) tagstatus tokens
                      | Delim (1, Doublequote) :: tokens ->
                          if debug then
                            eprintf "(OMD) (IHTML) non empty attribute 2 %S\n%!"
                              (L.string_of_tokens tokens);
                          begin match
                            fsplit
                              ~excl:(function
                                  | Delim (n, Doublequote) :: _ when n >= 2 ->
                                      true
                                  | _ ->
                                      false
                                )
                              ~f:(function
                                  | Delim (1, Doublequote) :: tl ->
                                      Split ([], tl)
                                  | _ ->
                                      Continue
                                ) tokens
                          with
                          | None ->
                              None
                          | Some (at_val, tokens) ->
                              if debug then
                                eprintf "(OMD) (3957) %s=%S %s\n%!" attributename
                                  (L.string_of_tokens at_val) (L.destring_of_tokens tokens);
                              loop body ((attributename, Some (L.string_of_tokens at_val)) :: attrs) tagstatus tokens
                          end
                      | _ ->
                          None
                      end
                  | Void, _ ->
                      None
                  end

              | Delim (n, Backslash) :: x :: tokens when (match tagstatus with T.Open _ :: _ -> true | _ -> false) && n mod 2 = 1 ->
                  loop (T.TOKENS [Delim (n, Backslash); x] :: body) attrs tagstatus tokens

              | x :: tokens when (match tagstatus with T.Open _ :: _ -> true | _ -> false) ->
                  if debug then eprintf "(OMD) (4161) general %S\n%!" (L.string_of_tokens (x::tokens));
                  loop (T.TOKENS [x] :: body) attrs tagstatus tokens
              | (Delim (1, Newline) | Delim (_, Space)) :: tokens when (match tagstatus with T.Awaiting _ :: _ -> true | _ -> false) ->
                  if debug then eprintf "(OMD) (4289) spaces\n%!";
                  loop body attrs tagstatus tokens
              | _ ->
                  if debug then
                    eprintf "(OMD) (4294) fallback with tokens=%s and tagstatus=%s\n%!"
                      (L.destring_of_tokens tokens) (T.string_of_tagstatus tagstatus);
                  begin match tagstatus with
                  | [] ->
                      Some (body, tokens)
                  | T.Awaiting tag :: _ ->
                      warn (sprintf "expected to read an open HTML tag (%s), but found nothing" tag);
                      None
                  | T.Open tag :: _ ->
                      warn (sprintf "expected to find the closing HTML tag for %s, but found nothing" tag);
                      None
                  end
            in match loop [] [] [] lexemes with
            | Some (html, rest) ->
                Some (T.md_of_interm_list html, rest)
            | None ->
                None
          in
          begin match read_html() with
          | Some(h, rest) ->
              main_impl_rev ~html (h@r) [Delim (1, Greaterthan)] rest
          | None ->
              let text = L.string_of_token t in
              main_impl_rev ~html (Text (text ^ tagnametop) :: r) [w] html_stuff
          end
    (* / end of inline HTML. *)

    (* < : emails *)
    | _, (Delim (1, Lessthan) as t) :: tl ->
        begin match maybe_autoemail r previous lexemes with
        | Some (r,p,l) ->
            main_impl_rev ~html r p l
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end

    (* line breaks *)
    | _, Delim (1, Newline) :: tl ->
        main_impl_rev ~html (NL :: r) [Delim (1, Newline)] tl
    | _, Delim (_, Newline) :: tl (* n >= 2 *) ->
        main_impl_rev ~html (NL :: NL :: r) [Delim (1, Newline)] tl

    (* [ *)
    | _, (Delim (1, Obracket) as t) :: tl ->
        begin match maybe_link main_loop r previous tl with
        | Some (r, p, l) ->
            main_impl_rev ~html r p l
        | None ->
            begin match maybe_reference r previous tl with
            | Some(r, p, l) ->
                main_impl_rev ~html r p l
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            end
        end

    (* img *)
    | _, (Delim (n, Exclamation) as t) :: Delim (1, Obracket) :: Delim (1, Cbracket) :: Delim (1, Oparenthesis) :: tl ->
        (* image insertion with no "alt" *)
        (* ![](/path/to/img.jpg) *)
        begin try
          let b, tl = read_until_cparenth ~bq:true ~no_nl:false tl in
          (* new lines there are allowed *)
          let r = (* updated result *)
            match n with
            | 1 -> r
            | n -> Text (String.make (n-1) '!') :: r
          in
          match
            try
              Some (read_until_space ~bq:false ~no_nl:true b)
            with Premature_ending ->
              None
          with
          | Some (url, tls) ->
              let title, _should_be_empty_list = read_until_dq ~bq:true (snd (read_until_dq ~bq:true tls)) in
              let url = L.string_of_tokens url in
              let title = L.string_of_tokens title in
              main_impl_rev ~html (Img ("", url, title) :: r) [Delim (1, Cparenthesis)] tl
          | None ->
              let url = L.string_of_tokens b in
              main_impl_rev ~html (Img ("", url, "") :: r) [Delim (1, Cparenthesis)] tl
        with NL_exception ->
          main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end

    (* img ref *)
    | _, (Delim (1, Exclamation) as t) :: Delim (1, Obracket) :: Delim (1, Cbracket) :: Delim (1, Obracket) :: tl ->
        (* ref image insertion with no "alt" *)
        (* ![][ref] *)
        begin try
          let id, tl = read_until_cbracket ~bq:true ~no_nl:true tl in
          let fallback = extract_fallback tl lexemes in
          let id = L.string_of_tokens id in
          main_impl_rev ~html (Img_ref (id, "", fallback) :: r) [Delim (1, Cbracket)] tl
        with NL_exception ->
          main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end

    (* img *)
    | _, (Delim (n, Exclamation) as t) :: Delim (1, Obracket) :: tl ->
        (* image insertion with "alt" *)
        (* ![Alt text](/path/to/img.jpg "Optional title") *)
        begin try
          match read_until_cbracket ~bq:true tl with
          | alt, Delim (1, Oparenthesis) :: ntl ->
              begin try
                let alt = L.string_of_tokens alt in
                let path_title, rest = read_until_cparenth ~bq:true ~no_nl:false ntl in
                let path, title =
                  try
                    read_until_space ~bq:true ~no_nl:true path_title
                  with Premature_ending ->
                    path_title, []
                in
                let title, nothing =
                  if title <> [] then
                    read_until_dq ~bq:true (snd(read_until_dq ~bq:true title))
                  else
                    [], []
                in
                if nothing <> [] then raise NL_exception; (* caught right below *)
                let r =
                  match n with
                  | 1 -> r
                  | n -> Text (String.make (n-1) '!') :: r
                in
                let path = L.string_of_tokens path in
                let title = L.string_of_tokens title in
                main_impl_rev ~html (Img (alt, path, title) :: r) [Delim (1, Cparenthesis)] rest
              with NL_exception | Premature_ending -> (* if NL_exception was raised, then fall back to "text" *)
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
              end
          | alt, Delim (1, Obracket) :: Word id :: Delim (1, Cbracket) :: ntl
          | alt, Delim (1, Obracket) :: Delim (_, Space) :: Word id :: Delim (1, Cbracket) :: ntl
          | alt, Delim (1, Obracket) :: Delim (_, Space) :: Word id :: Delim (_, Space) :: Delim (1, Cbracket) :: ntl
          | alt, Delim (1, Obracket) :: Word id :: Delim (_, Space) :: Delim (1, Cbracket) :: ntl ->
              let fallback = extract_fallback ntl lexemes in
              let alt = L.string_of_tokens alt in
              main_impl_rev ~html (Img_ref(id, alt, fallback) :: r) [Delim (1, Cbracket)] ntl
          | alt, Delim (1, Obracket) :: ((Delim (1, Newline) | Delim (_, Space) | Word _ | Number _) :: _ as ntl) ->
              begin try
                match read_until_cbracket ~bq:true ~no_nl:false ntl with
                | [], _rest ->
                    raise Premature_ending
                | id, rest ->
                    let fallback = extract_fallback rest lexemes in
                    let id = L.string_of_tokens id in
                    let alt = L.string_of_tokens alt in
                    main_impl_rev ~html (Img_ref (id, alt, fallback)::r) [Delim (1, Cbracket)] rest
              with Premature_ending | NL_exception ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
              end
          | _ ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        with Premature_ending ->
          main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        end

    | _,
      (Delim (1, (At|Bar|Caret|Cbrace|Colon|Comma|Cparenthesis|Cbracket|Dollar
                 |Dot|Doublequote|Exclamation|Equal|Minus|Obrace|Oparenthesis
                 |Percent|Plus|Question|Quote|Semicolon|Slash|Tab|Tilde
                 |Greaterthan)) as t) :: tl ->
        main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
    | _, (Number _  as t):: tl ->
        main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl

    | _, (Delim (_, (At|Bar|Caret|Cbrace|Cbracket|Colon
                    |Comma|Cparenthesis|Dollar|Dot|Doublequote
                    |Equal|Exclamation|Greaterthan|Lessthan
                    |Minus|Obrace|Obracket|Oparenthesis
                    |Percent|Plus|Question|Quote|Semicolon
                    |Slash|Star|Tab|Tilde|Underscore)) as tk) :: tl ->
        let tk0, tks = L.split_first tk in
        let text = L.string_of_token tk0 in
        main_impl_rev ~html (Text text :: r) [tk0] (tks :: tl)

  and main_impl ~html r previous lexemes =
    (* if debug then eprintf "(OMD) main_impl html=%b\n%!" html; *)
    assert_well_formed lexemes;
    Cat (List.rev (main_loop_rev ~html r previous lexemes))

  and main_loop ?(html = false) r previous lexemes =
    main_impl ~html r previous lexemes

  and main_loop_rev ?(html = false) r previous lexemes =
    main_impl_rev ~html r previous lexemes

  let parse lexemes =
    main_loop [] [] lexemes
end

module P = struct
  let code n p =
    let b = Buffer.create 18 in
    let rec f p =
      match Sub.head p with
      | Some ('`', p) ->
          let rec loop m p =
            if m = 0 then
              Some (Buffer.contents b, p)
            else begin
              match Sub.head p with
              | Some ('`', p) ->
                  loop (pred m) p
              | Some _ ->
                  for _ = 1 to n - m do Buffer.add_char b '`' done;
                  f p
              | None ->
                  None
            end
          in
          loop (pred n) p
      | Some (c, p) ->
          Buffer.add_char b c;
          f p
      | None ->
          None
    in
    f p

  let f p =
    let b = Buffer.create 18 in
    let get acc =
      if Buffer.length b = 0 then
        acc
      else begin
        let s = Buffer.contents b in
        Buffer.clear b;
        Text s :: acc
      end
    in
    let rec f acc p =
      match Sub.head p with
      | Some ('\\', p) ->
          begin match Sub.head p with
          | Some (c, p) ->
              Buffer.add_char b c;
              f acc p
          | None ->
              f acc p
          end
      | Some ('`', p) ->
          let rec loop n p =
            match Sub.head p with
            | Some ('`', p) ->
                loop (succ n) p
            | Some _ ->
                begin match code n p with
                | Some (s, p) ->
                    f (Code s :: get acc) p
                | None ->
                    Buffer.add_string b (String.make n '`');
                    f acc p
                end
            | None ->
                Buffer.add_string b (String.make n '`');
                f acc p
          in
          loop 1 p
      (* | Some ('<', p) -> *)
      (*     begin match autolink p with *)
      (*     | Some (x, p) -> *)
      (*         f (Url x :: acc) p *)
      (*     | None -> *)
      (*         begin match html_tag p with *)
      (*         | Some x -> *)
      (*             f (Html x :: acc) p *)
      (*         | None -> *)
      (*             f acc p *)
      (*         end *)
      (*     end *)
      | Some (c, p) ->
          Buffer.add_char b c;
          f acc p
      | None ->
          List.rev (get acc)
    in
    f [] p
end

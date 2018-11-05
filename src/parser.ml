(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013-2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Printf
open Representation
open Utils

module L = Lexer

type r = Representation.t
(* accumulator (beware, reversed tokens) *)

and p = Representation.tok list
(* context information: previous elements *)

and l = Representation.tok list
(* tokens to parse *)

and main_loop =
  ?html:bool ->
  r -> (* accumulator (beware, reversed tokens) *)
  p -> (* info: previous elements *)
  l -> (* tokens to parse *)
  Representation.t (* final result *)
(* most important loop *)

(* N.B. Please do not use tabulations in your Markdown file! *)

module type Env = sig
  val rc: Representation.ref_container
  val extensions: Representation.extensions
  val default_lang: string
  val gh_uemph_or_bold_style: bool
  val blind_html: bool
  val strict_html: bool
  val warning: bool
  val warn_error: bool
end

module Unit = struct end

module Default_env (Unit : sig end) : Env = struct
  let rc = new Representation.ref_container
  let extensions = []
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

  (* Remove all [NL] and [Br] at the beginning. *)
  let rec remove_initial_newlines = function
    | [] -> []
    | (NL | Br) :: tl -> remove_initial_newlines tl
    | l -> l

  (* - recognizes paragraphs
     - glues following blockquotes  *)
  let make_paragraphs md =
    let rec loop cp accu = function (* cp means current paragraph *)
      | [] ->
          let accu =
            match cp with
            | [] | [NL] | [Br] ->
                accu
            | (NL | Br) :: cp ->
                Paragraph (List.rev cp) :: accu
            | cp ->
                Paragraph (List.rev cp) :: accu
          in
          List.rev accu
      | Blockquote b1 :: Blockquote b2 :: tl ->
          loop cp accu (Blockquote (b1@b2) :: tl)
      | Blockquote b :: tl ->
          let e = Blockquote (loop [] [] b) in
          begin match cp with
          | [] | [NL] | [Br] ->
              loop cp (e :: accu) tl
          | _ ->
              loop [] (e :: Paragraph (List.rev cp) :: accu) tl
          end
      | (Ulp b) :: tl ->
          let e = Ulp (List.map (fun li -> loop [] [] li) b) in
          begin match cp with
          | [] | [NL] | [Br] ->
              loop cp (e :: accu) tl
          | _ ->
              loop [] (e :: Paragraph (List.rev cp) :: accu) tl
          end
      | (Olp b) :: tl ->
          let e = Olp (List.map (fun li -> loop [] [] li) b) in
          begin match cp with
          | [] | [NL] | [Br] ->
              loop cp (e :: accu) tl
          | _ ->
              loop [] (e :: Paragraph (List.rev cp) :: accu) tl
          end
      | Html_comment _ as e :: tl ->
          begin match cp with
          | [] ->
              loop [] (e :: accu) tl
          | [NL] | [Br] ->
              loop [] (e :: NL :: accu) tl
          | _ ->
              loop (e :: cp) accu tl
          end
      | (Raw_block _ | Html_block _) as e :: tl ->
          begin match cp with
          | [] | [NL] | [Br] ->
              loop cp (e :: cp@accu) tl
          | _ ->
              loop [] (e :: Paragraph (List.rev cp) :: accu) tl
          end
      | (Code_block _ | H _ | Ol _ | Ul _) as e :: tl ->
          begin match cp with
          | [] | [NL] | [Br] ->
              loop cp (e :: accu) tl
          | _ ->
              loop [] (e :: Paragraph (List.rev cp) :: accu) tl
          end
      | Text "\n" :: _ | Paragraph _ :: _ ->
          invalid_arg "Omd_parser.make_paragraphs"
      | (NL | Br) :: (NL | Br) :: tl ->
          let tl = remove_initial_newlines tl in
          begin match cp with
          | [] | [NL] | [Br] -> loop [] (NL::NL::accu) tl
          | _ -> loop [] (Paragraph(List.rev cp)::accu) tl
          end
      | X(x) as e :: tl ->
          (* If the extension returns a block as first element,
             then consider the extension as a block. However
             don't take its contents as it is yet, the contents
             of the extension shall be considered final as late
             as possible. *)
          begin match x#to_t md with
          | None ->
              loop (e :: cp) accu tl
          | Some ((H _|Paragraph _|
                   Ul _|Ol _|Ulp _|Olp _|Code_block _|Hr|Html_block _|Raw_block _| Blockquote _) :: _) ->
              begin match cp with
              | [] | [NL] | [Br] ->
                  loop cp (e::accu) tl
              | _ ->
                  loop [] (e::Paragraph(List.rev cp)::accu) tl
              end
          | Some _ ->
              loop (e::cp) accu tl
          end
      | e::tl ->
          loop (e::cp) accu tl
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
    let rec clean_paragraphs =
      if debug then eprintf "(OMD) clean_paragraphs\n";
      function
      | [] -> []
      | Paragraph [] :: tl ->
          tl
      | Paragraph (p) :: tl ->
          Paragraph (clean_paragraphs (remove_initial_newlines (remove_white_crumbs (normalise_md p)))) ::
          clean_paragraphs tl
      | H (i, v) :: tl ->
          H (i, clean_paragraphs v) :: clean_paragraphs tl
      | Emph v :: tl ->
          Emph (clean_paragraphs v) :: clean_paragraphs tl
      | Bold v :: tl ->
          Bold (clean_paragraphs v) :: clean_paragraphs tl
      | Ul v :: tl ->
          Ul (List.map clean_paragraphs v) :: clean_paragraphs tl
      | Ol v :: tl ->
          Ol (List.map clean_paragraphs v) :: clean_paragraphs tl
      | Ulp v :: tl ->
          Ulp (List.map clean_paragraphs v) :: clean_paragraphs tl
      | Olp v :: tl ->
          Olp (List.map clean_paragraphs v) :: clean_paragraphs tl
      | Blockquote v :: tl ->
          Blockquote (clean_paragraphs v) :: clean_paragraphs tl
      | Url (href, v, title) :: tl ->
          Url (href, clean_paragraphs v, title) :: clean_paragraphs tl
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
      | X _ as v :: tl ->
          v :: clean_paragraphs tl
    in
    let r = clean_paragraphs(loop [] [] md) in
    if debug then eprintf "(OMD) clean_paragraphs %S --> %S\n%!"
        (Backend.sexpr_of_md md) (Backend.sexpr_of_md r);
    r

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
  let extract_fallback _main_loop remains l =
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
      method to_t = [Text (L.string_of_tokens a)]
    end

  let unindent_rev n lexemes =
    if debug then eprintf "(OMD) CALL: Omd_parser.unindent_rev\n%!";
    assert_well_formed lexemes;
    let rec loop accu cl = function
      | Delim (x, Newline) :: Delim (_, Space) :: Delim (y, Newline) :: tl ->
          loop accu cl (Delim (x+y, Newline) :: tl)
      | (Delim ((1 | 2), Newline) as nl) :: (Delim (_, Space) as s) ::
        ((Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _) |
         (Delim (1, (Star | Plus | Minus)) :: Delim (_, Space) :: _) as tl) as l ->
          if n = L.length s then
            loop (nl::cl@accu) [] tl
          else
            (cl@accu), l
      | (Delim ((1|2), Newline) as nl) :: (Delim (_, Space) as s) :: tl ->
          let x = L.length s - n in
          loop (nl :: cl@accu)
            (if x > 0 then [L.make_space x] else [])
            tl
      | Delim (_, Newline) :: _ as l ->
          (cl@accu), l
      | e :: tl ->
          loop accu (e :: cl) tl
      | [] as l ->
          (cl@accu), l
    in
    match loop [] [] lexemes with
    | [], right ->
        [], right
    | l, right ->
        assert_well_formed l;
        l, right

  let unindent n lexemes =
    let fst, snd = unindent_rev n lexemes in
    List.rev fst, snd

  let rec is_blank = function
    | Delim (_, (Space | Newline)) :: tl ->
        is_blank tl
    | [] ->
        true
    | _ ->
        false

  let delim n d l = if n = 0 then l else Delim (n, d) :: l

  let semph_or_bold (n:int) (l:l) =
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

  let sm_uemph_or_bold (n:int) (l:l) =
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

  let gh_uemph_or_bold (n:int) (l:l) =
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

  (* used by tag__maybe_h1 and tag__maybe_h2 *)
  let setext_title main_loop (l:l) : (Representation.tok list * l) option =
    assert_well_formed l;
    let rec detect_balanced_bqs n r l =
      (* If there's a balanced (complete) backquote-started code block
         then it should be "ignored", else it means the line that
         follows is part of a code block, so it's not defining a
         setext-style title. *)
      if debug then
        eprintf "(OMD) detect_balanced_bqs n=%d r=%S l=%S\n%!"
          n (L.string_of_tokens r) (L.string_of_tokens l);
      match l with
      | [] ->
          None
      | Delim (_, Newline) ::_ ->
          None
      | Delim (m, Backslash) as b :: Delim (x, Backquote) :: tl when m mod 2 = 1 ->
          detect_balanced_bqs n (Delim (1, Backquote) :: b ::r) (delim (x-1) Backquote tl)
      | Delim (x, Backquote) as b :: tl when n = x ->
          Some (List.rev (b :: r), tl)
      | e :: tl ->
          detect_balanced_bqs n (e :: r) tl
    in
    let rec loop r = function
      | [] ->
          if r = [] then
            None
          else
            Some (List.rev r, [])
      | Delim (m, Backslash) as b :: Delim (x, Backquote) :: tl when m mod 2 = 1 ->
          loop (Delim (1, Backquote) :: b :: r) (delim (x-1) Backquote tl)
      | Delim (x, Backquote) :: tl ->
          begin match detect_balanced_bqs x [] tl with
          | Some(bl,tl) -> loop (bl@r) tl
          | _ -> None
          end
      | Delim (1, Newline) :: Delim (_, (Equal | Minus)) :: tl ->
          if r = [] then
            None
          else
            Some (List.rev r, tl)
      | Delim (_, Newline) :: _ ->
          if debug then
            eprintf "(OMD) Omd_parser.setext_title is wrongly used!\n%!";
          None
      | e :: tl ->
          loop (e :: r) tl
    in
    if match l with
      | Delim (1, Lessthan) :: Word _ :: _ ->
          begin match main_loop [] [] l with
          | (Html_block _ | Code_block _ | Raw_block _) :: _ ->
              true
          | _ ->
              false
          end
      | _ ->
          false
    then
      None
    else
      let result = loop [] l in
      if debug then
        eprintf "(OMD) setext_title l=%S result=%S,%S\n%!"
          (L.string_of_tokens l)
          (match result with
           | None -> ""
           | Some (x, _) -> L.string_of_tokens x)
          (match result with
           | None -> ""
           | Some (_, tl) -> L.string_of_tokens tl);
      result

  let tag__maybe_h1 (main_loop:main_loop) =
    Tag("tag__maybe_h1",
        object
          method parser_extension r p l =
            match p with
            | [] | [Delim (_, Newline)] ->
                begin match setext_title main_loop l with
                | None ->
                    None
                | Some (title, tl) ->
                    let title = H (1, main_loop [] [] title) in
                    Some (title :: r, [Delim (1, Newline)], tl)
                end
            | _ ->
                if debug then
                  eprintf "(OMD) Warning: Omd_parser.tag__maybe_h1 is wrongly \
                           used (p=%S)!\n"
                    (L.string_of_tokens p);
                None
          method to_string = ""
        end
       )

  let tag__maybe_h2 (main_loop:main_loop) =
    Tag("tag__maybe_h2",
        object
          method parser_extension r p l =
            match p with
            | [] | [Delim (_, Newline)] ->
                begin match setext_title main_loop l with
                | None ->
                    None
                | Some (title, tl) ->
                    let title = H (2, main_loop [] [] title) in
                    Some (title :: r, [Delim (1, Newline)], tl)
                end
            | _ ->
                if debug then
                  eprintf "(OMD) Warning: Omd_parser.tag__maybe_h2 is wrongly \
                           used (p=%S)!\n"
                    (L.string_of_tokens p);
                None
          method to_string = ""
        end
       )

  let tag__md md = (* [md] should be in reverse *)
    Tag("tag__md",
        object
          method parser_extension r _p l = Some(md@r, [], l)
          method to_string = ""
        end
       )

  (* Let's tag the lines that *might* be titles using setext-style.
     "might" because if they are, for instance, in a code section,
     then they are not titles at all. *)
  let tag_setext main_loop lexemes =
    assert_well_formed lexemes;
    let rec loop pl res = function
      | [] | [Delim (_, Newline)] ->
          pl@res
      | Delim (1, Newline) as e1 :: (Delim (_, Equal) as e2) :: tl -> (* might be a H1. *)
          begin match
            fsplit_rev
              ~f:(function
                  | Delim (_, (Space | Equal)) :: _ ->
                      Continue
                  | [] ->
                      Split ([],[])
                  | _::_ as l ->
                      Split ([], l)
                ) tl
          with
          | Some (rleft, (([] | Delim (_, Newline) ::_) as right)) ->
              loop [] (rleft@(e2::e1::pl@tag__maybe_h1 main_loop::res)) right
          | Some (rleft, right) ->
              loop [] (rleft@(e2::e1::pl@res)) right
          | None ->
              loop [] (e2::e1::pl@res) []
          end
      | Delim (1, Newline) as e1 :: (Delim (_, Minus) as e2) :: tl -> (* might be a H2. *)
          begin match
            fsplit_rev
              ~f:(function
                  | Delim (_, (Space | Minus)) :: _ ->
                      Continue
                  | [] ->
                      Split ([], [])
                  | _ :: _ as l ->
                      Split ([], l)
                ) tl
          with
          | Some (rleft, (([] | Delim (_, Newline) :: _) as right)) ->
              loop [] (rleft@(e2::e1::pl@tag__maybe_h2 main_loop::res)) right
          | Some(rleft, right) ->
              loop [] (rleft@(e2::e1::pl@res)) right
          | None ->
              loop [] (e2::e1::pl@res) []
          end
      | Delim (_, Newline) as e1 :: tl ->
          loop [] (e1::pl@res) tl
      | e::tl ->
          loop (e::pl) res tl
    in
    List.rev (loop [] [] lexemes)

  let hr_m l =
    assert_well_formed l;
    let rec loop n = function
      | Delim (_, Newline) :: tl | ([] as tl) ->
          if n >= 3 then Some tl else None
      | Delim (_, Space) :: tl ->
          loop n tl
      | Delim (x, Minus) :: tl ->
          loop (x+n) tl
      | _::_ ->
          None
    in
    loop 0 l

  let hr_s l =
    assert_well_formed l;
    let rec loop n = function
      | Delim (_, Newline) :: tl | ([] as tl) ->
          if n >= 3 then Some tl else None
      | Delim (_, Space) :: tl ->
          loop n tl
      | Delim (x, Star) :: tl ->
          loop (x+n) tl
      | _ :: _ ->
          None
    in
    loop 0 l

  let hr l =
    match hr_m l with
    | None -> hr_s l
    | Some _ as tl -> tl

  (* [bcode] parses code that's delimited by backquote(s) *)
  let bcode ?(default_lang = default_lang) r p l =
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
        if List.exists (function Delim (_, Newline) -> true | _ -> false) cb
        && (match p with [] | [Delim (_, Newline)] -> true | _ -> false)
        && (match e with Delim (n, Backquote) -> n > 0 | _ -> false)
        then
          match cb with
          | Word lang :: Delim (_, Space) :: Delim (n, Newline) :: tl
          | Word lang :: Delim (n, Newline) :: tl ->
              let code = L.string_of_tokens (delim (n-1) Newline tl) in
              Some (Code_block (lang, code) :: r, [Delim (1, Backquote)], l)
          | Delim (1, Newline) :: tl ->
              let code = L.string_of_tokens tl in
              Some (Code_block (default_lang, code) :: r, [Delim (1, Backquote)], l)
          | _ ->
              let code = L.string_of_tokens cb in
              Some (Code_block (default_lang, code) :: r, [Delim (1, Backquote)], l)
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
          let code = L.string_of_tokens cb in
          if debug then
            eprintf "(OMD) clean_bcode %S => %S\n%!" code (clean_bcode code);
          Some (Code (default_lang, clean_bcode code) :: r, [Delim (1, Backquote)], l)

  exception NL_exception
  exception Premature_ending

  (* !!DO NOT DELETE THIS!!
     The program that generates the generated part that follows right after.
     List.iter (fun (a,b,c) ->
     print_endline ("let read_until_"^a^" ?(bq=false) ?(no_nl=false) l =
     assert_well_formed l;
     let rec loop accu n = function
      | Backslash :: ("^b^" as b) :: tl ->
        loop (b::accu) n tl
      | Backslash :: ("^b^"s 0) :: tl ->
        loop ("^b^"::accu) n ("^b^"::tl)
      | Backslashs 0 :: tl ->
        loop (Backslash::accu) n tl
      | Backslashs 1 :: tl ->
        loop (Backslash::accu) n (Backslash::tl)
      | Backslashs 2 :: tl ->
        loop (Backslashs 0::accu) n tl
      | (Backslashs x) :: tl ->
        if x mod 2 = 0 then
          loop (Backslashs(x/2-1)::accu) n tl
        else
          loop (Backslashs(x/2-1)::accu) n (Backslash::tl)
      | (Backquote|Backquotes _ as e)::tl as l ->
        if bq then
          match bcode [] [] l with
          | None -> loop (e::accu) n tl
          | Some (r, _, tl) ->
            loop (* not very pretty kind of hack *)
              (List.rev(L.lex(Omd_backend.markdown_of_md r))@accu)
              n
              tl
        else
         loop (e::accu) n tl"
      ^(if c<>"" then "
      | Backslash :: ("^c^" as b) :: tl ->
        loop (b::accu) n tl
      | Backslash :: ("^c^"s 0) :: tl ->
        loop ("^c^"::accu) n ("^c^"::tl)
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
      | "^b^"s x :: tl ->
        if n = 0 then
          List.rev accu, "^b^"s(x-1)::tl
        else
          loop
            (match accu with
             | "^b^"::accu -> "^b^"s(0)::accu
             | "^b^"s x::accu -> "^b^"s(x+1)::accu
             | _ -> "^b^"::accu)
            (n-1)
            ("^b^"s(x-1)::tl)
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
       eprintf \"Omd_parser.read_until_"^a^" %S bq=%b no_nl=%b\\n%!\" (L.string_of_tokens l) bq no_nl;
     let res = loop [] 0 l in
     if debug then
       eprintf \"Omd_parser.read_until_"^a^" %S bq=%b no_nl=%b => %S\\n%!\" (L.string_of_tokens l) bq no_nl (L.string_of_tokens (fst res));
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
                  (List.rev (L.lex (Backend.markdown_of_md r))@accu)
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

  let read_until_gt ?bq ?no_nl l =
    read_until Greaterthan (Some Lessthan) ?bq ?no_nl l

  let read_until_lt ?bq ?no_nl l =
    read_until Lessthan None ?bq ?no_nl l

  let read_until_cparenth ?bq ?no_nl l =
    read_until Cparenthesis (Some Oparenthesis) ?bq ?no_nl l

  let read_until_oparenth ?bq ?no_nl l =
    read_until Oparenthesis None ?bq ?no_nl l

  let read_until_dq ?bq ?no_nl l =
    read_until Doublequote None ?bq ?no_nl l

  let read_until_q ?bq ?no_nl l =
    read_until Quote None ?bq ?no_nl l

  let read_until_obracket ?bq ?no_nl l =
    read_until Obracket None ?bq ?no_nl l

  let read_until_cbracket ?bq ?no_nl l =
    read_until Cbracket (Some Obracket) ?bq ?no_nl l

  let read_until_space ?bq ?no_nl l =
    read_until Space None ?bq ?no_nl l

  let read_until_newline l =
    assert_well_formed l;
    let rec loop accu n = function
      | Delim (1, Backslash) :: Delim (x, Newline) :: tl ->
          loop (Delim (1, Newline) :: Delim (1, Backslash) :: accu) n (delim (x-1) Newline tl)
      | Delim (2, Backslash) as e :: tl ->
          loop (e :: accu) n tl
      | Delim (x, Backslash) :: tl ->
          loop (delim (x-x mod 2) Backslash accu) n (delim (x mod 2) Backslash tl)
      | Delim ((1|2) as x, Newline) :: tl ->
          if n = 0 then
            List.rev accu, delim (x-1) Newline tl
          else
            loop (Delim (1, Newline) :: accu) (n - 1) (delim (x-1) Newline tl) (* FIXME ?? *)
      | Delim (x, Newline) :: tl ->
          List.rev accu, delim (x-1) Newline tl
      | e :: tl ->
          loop (e :: accu) n tl
      | [] ->
          raise Premature_ending
    in
    loop [] 0 l

  (* H1, H2, H3, ... *)
  let read_title (main_loop:main_loop) n r _previous lexemes =
    let title, rest =
      let rec loop accu = function
        | Delim (n, Backslash) :: Delim (h, Hash) ::
          ([] | Delim (_, Space) :: Delim (_, Newline) :: _ | Delim (_, Newline) :: _ as tl) when n mod 2 = 1 ->
            loop (Delim (1, Hash) :: Delim (n, Backslash) :: accu) (delim (h-1) Hash tl)
        | Delim (n, Backslash) :: Delim (h, Hash) :: tl when n mod 2 = 1 ->
            loop (Delim (h, Hash) :: Delim (n, Backslash) :: accu) tl
        | Delim (_, Hash) :: (Delim (_, Newline) :: _ as l)
        | Delim (_, Hash) :: Delim (_, Space) :: (Delim (_, Newline) :: _ as l)
        | (Delim (_, Newline) :: _ as l)
        | ([] as l)
        | Delim (_, Space) :: Delim (_, Hash) :: (Delim (_, Newline) :: _ as l)
        | Delim (_, Space) :: Delim (_, Hash) :: Delim (_, Space) :: (Delim (_, Newline) :: _ as l)
        | Delim (_, Space) :: (Delim (_, Newline) :: _ as l)
        | Delim (_, Space) :: ([] as l) ->
            main_loop [] [] (List.rev accu), l
        | [Delim (_, Hash)]
        | [Delim (_, Space); Delim (_, Hash)]
        | [Delim (_, Space); Delim (_, Hash); Delim (_, Space)] ->
            main_loop [] [] (List.rev accu), []
        | x::tl ->
            loop (x::accu) tl
      in
      loop [] lexemes
    in
    match n with
    | 1 | 2 | 3 | 4 | 5 | 6 -> Some (H (n, title) :: r, [Delim (1, Newline)], rest)
    | _ -> None

  let maybe_extension extensions r p l =
    match extensions with
    | [] ->
        None
    | _ ->
        List.fold_left (function
            | None ->
                (fun f -> f#parser_extension r p l)
            | Some(nr, np, nl) as e ->
                (fun f ->
                   match f#parser_extension nr np nl with
                   | None -> e
                   | Some _ as k -> k
                )
          ) None extensions

  (* blockquotes *)
  let emailstyle_quoting (main_loop:main_loop) r _p lexemes =
    assert_well_formed lexemes;
    let rec loop block cl = function
      | Delim (1, Newline) :: Delim (1, Greaterthan) :: (Delim (1, Newline) :: _ as tl) ->
          loop (Delim (1, Newline) :: cl@block) [] tl
      | Delim (1, Newline) :: Delim (1, Greaterthan) :: Delim (n, Space) :: tl ->
          assert (n > 0);
          loop (Delim (1, Newline) :: cl@block) (delim (n-1) Space []) tl
      (* multi paragraph blockquotes with empty lines *)
      | Delim (2, Newline) :: Delim (1, Greaterthan) :: Delim (n, Space) :: tl ->
          assert(n>0);
          loop (Delim (2, Newline) :: cl@block) (delim (n-1) Space []) tl
      | (Delim (_, Newline) :: _ as l) | ([] as l) ->
          fix (List.rev (cl@block)), l
      | e :: tl ->
          loop block (e :: cl) tl
    in
    match loop [] [] lexemes with
    | Delim (_, Newline) :: block, tl ->
        if debug then
          eprintf "(OMD) Omd_parser.emailstyle_quoting %S\n%!"
            (L.string_of_tokens block);
        Some (Blockquote (main_loop [] [] block) :: r, [Delim (1, Newline)], tl)
    | _ ->
        None

  (* maybe a reference *)
  let maybe_reference (main_loop:main_loop) rc r _p l =
    assert_well_formed l;
    (* this function is called when we know it's not a link although
       it started with a '[' *)
    (* So it could be a reference or a link definition. *)
    let maybe_ref l =
      let text, remains = read_until_cbracket ~bq:true l in
      let blank, remains = read_until_obracket ~bq:true remains in
      (* check that there are no unwanted characters between CB and OB. *)
      if eat (let flag = ref true in
              function (* allow only a space, multiple spaces, or a newline *)
              | Delim (1, Newline) -> !flag && (flag := false; true)
              | Delim (_, Space) -> !flag && (flag := false; true)
              | _ -> false) blank <> [] then
        raise Premature_ending (* <-- not a regular reference *)
      else begin
        let contents = main_loop [] [] text in
        if (* if [contents] contains a link, we stop here! *)
          let _ =
            visit (function Url _ -> raise Premature_ending| _ -> None) contents
          in false
        then raise Premature_ending;
        match read_until_cbracket ~bq:true remains with
        | [], remains ->
            let fallback = extract_fallback main_loop remains (Delim (1, Obracket) :: l) in
            let id = L.string_of_tokens text in (* implicit anchor *)
            Some (Ref (rc, id, contents, fallback) :: r, [Delim (1, Cbracket)], remains)
        | id, remains ->
            let fallback = extract_fallback main_loop remains (Delim (1, Obracket) :: l) in
            Some(Ref (rc, L.string_of_tokens id, contents, fallback) :: r, [Delim (1, Cbracket)], remains)
      end
    in
    let maybe_nonregular_ref l =
      let text, remains = read_until_cbracket ~bq:true l in
      (* check that there is no ill-placed open bracket *)
      if (match read_until_obracket ~bq:true text with _ -> true | exception Premature_ending -> false) then
        raise Premature_ending; (* <-- ill-placed open bracket *)
      let fallback = extract_fallback main_loop remains (Delim (1, Obracket) :: l) in
      let id = L.string_of_tokens text in (* implicit anchor *)
      let contents = main_loop [] [] text in
      Some (Ref (rc, id, contents, fallback) :: r, [Delim (1, Cbracket)], remains)
    in
    let maybe_def l =
      match read_until_cbracket ~bq:true l with
      | _, [] ->
          raise Premature_ending
      | id, (Delim (1, Colon) :: Delim (_, Space) :: remains)
      | id, (Delim (1, Colon) :: remains) ->
          begin match
            fsplit
              ~f:(function
                  | Delim (_, (Space | Newline)) :: _ as l ->
                      Split ([], l)
                  | _ :: _ ->
                      Continue
                  | [] ->
                      Split ([], [])
                ) remains
          with
          | None | Some([], _) ->
              raise Premature_ending
          | Some (url, remains) ->
              let title, remains =
                match eat (function Delim (_, (Space | Newline)) -> true | _ -> false) remains with
                | Delim (2, Doublequote) :: tl ->
                    [], tl
                | Delim (1, Doublequote) :: tl ->
                    read_until_dq ~bq:true tl
                | Delim (2, Quote) :: tl ->
                    [], tl
                | Delim (1, Quote) :: tl ->
                    read_until_q ~bq:true tl
                | Delim (1, Oparenthesis) :: tl->
                    read_until_cparenth ~bq:true tl
                | l ->
                    [], l
              in
              let url =
                let url = L.string_of_tokens url in
                if String.length url > 2 && url.[0] = '<' && url.[String.length url - 1] = '>' then
                  String.sub url 1 (String.length url - 2)
                else
                  url
              in
              rc#add_ref (L.string_of_tokens id) (L.string_of_tokens title) url;
              Some(r, [Delim (1, Newline)], remains)
          end
      | _ ->
          raise Premature_ending
    in
    try
      maybe_ref l
    with Premature_ending | NL_exception ->
    try
      maybe_def l
    with Premature_ending | NL_exception ->
    try
      maybe_nonregular_ref l
    with Premature_ending | NL_exception ->
      None

  (* maybe a link *)
  let maybe_link (main_loop:main_loop) r _p l =
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

  let has_paragraphs l =
    (* Has at least 2 consecutive newlines. *)
    List.exists (function Delim (_, Newline) -> true | _ -> false) l

  let parse_list (main_loop:main_loop) r _p l =
    assert_well_formed l;
    if debug then begin
      eprintf "(OMD) parse_list r=(%s) p=(%s) l=(%s)\n%!"
        "" (* (Omd_backend.sexpr_of_md (List.rev r)) *)
        "" (* (destring_of_tl p) *)
        (L.destring_of_tokens ~limit:40 l);
    end;
    let module UO = struct type ordered = O | U end in
    let open UO in
    if debug then eprintf "(OMD) parse_list: l=(%s)\n%!" (L.destring_of_tokens l);
    let end_of_item (indent:int) l : tok split_action  =
      match l with
      | [] ->
          Split ([], [])
      | Delim (2, Newline) :: (Delim (n, Space) :: Delim (1, Greaterthan) :: Delim (_, Space) :: _ as s) when n >= 2 ->
          if n = indent+4 then (* blockquote *)
            match unindent n (Delim (1, Newline) :: s) with
            | Delim (n, Newline) :: block, rest ->
                Continue_with (List.rev (Delim (n+2, Newline) :: block), rest)
            | block, rest ->
                Continue_with (Delim (2, Newline) :: block, rest)
          else if n >= indent+8 then (* code inside item *)
            match unindent (indent+4) (Delim (1, Newline) :: s) with
            | Delim (n, Newline) :: block, rest ->
                Continue_with (List.rev (Delim (n+2, Newline) :: block), rest)
            | block, rest ->
                Continue_with (Delim (2, Newline) :: block, rest)
          else
            Split ([], l)
      | Delim (2, Newline) :: (Delim (n, Space) :: _ as s) when n >= 2 ->
          assert (n >= 0);
          if n >= indent+8 then (* code inside item *)
            match unindent (indent+4) (Delim (1, Newline) :: s) with
            | Delim (n, Newline) :: block, rest ->
                Continue_with (List.rev (Delim (n+1, Newline) :: block), rest)
            | block, rest ->
                Continue_with (Delim (1, Newline) :: block, rest)
          else if n >= indent+4 then (* new paragraph inside item *)
            match unindent (indent+4) (Delim (1, Newline) :: s) with
            | Delim (n, Newline) :: block, rest ->
                Continue_with (List.rev (Delim (n+2, Newline) :: block), rest)
            | block, rest ->
                Continue_with (Delim (2, Newline) :: block, rest)
          else
            Split ([], l)
      | Delim (n, Newline) :: _ when n >= 2 ->
          (* End of item, stop *)
          Split ([], l)
      | Delim (1, Newline) ::
        ( Delim (_, Space) :: Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: _
        | Delim (_, Space) :: Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _
        | Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: _
        | Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _ as tl) ->
          Split ([Delim (1, Newline)], tl)
      | Delim (1, Newline) :: Delim (_, Space) :: Delim (1, Newline) :: tl ->
          (* A line with spaces shouldn't interfere here,
             which is about exactly 2 consecutive newlines,
             so we rewrite the head of the lexing stream. *)
          Continue_with ([], Delim (2, Newline) :: tl)
      | Delim (1, Newline) :: Delim (_, Space) :: Delim (n, Newline) :: _ when n >= 2 ->
          (* A line with spaces shouldn't interfere here,
             which is about at least 3 consecutive newlines,
             so we stop. *)
          Split ([], l)
      | Delim (1, Newline) :: (Delim (n, Space) as s) :: tl when n >= 2 ->
          Continue_with
            ([s;
              Tag("parse_list/remember spaces",
                  object
                    method parser_extension r p = function
                      | Delim (n, Space) :: tl when n >= 2 ->
                          Some(r, p, Delim (1, Space) :: tl)
                      | _ ->
                          None

                    method to_string = ""
                  end);
              Delim (1, Newline)], tl)
      | Delim (1, Newline) :: (Delim (1, Space) as s) :: tl ->
          Continue_with
            ([s;
              Tag("parse_list/remember space",
                  object
                    method parser_extension r p = function
                      | Delim (_, Space) :: tl ->
                          Some (r,p, Delim (1, Space) :: tl)
                      | _ ->
                          None

                    method to_string = ""
                  end);
              Delim (1, Newline)], tl)
      | _::_ ->
          Continue
    in
    let rev_to_t l =
      assert_well_formed l;
      (* Newlines at the end of items have no meaning (except to end the
         item which is expressed by the constructor already). *)
      let l = match l with Delim (_, Newline) :: tl -> tl | _ -> l in
      main_loop [] [Delim (1, Newline)] (List.rev l)
    in
    let add (sublist:element) items =
      if debug then eprintf "(OMD) add\n%!";
      match items with
      | [] ->
          assert false
      | (O, indents, item)::tl ->
          (O, indents, (item@[sublist])) :: tl
      | (U, indents, item)::tl ->
          (U, indents, (item@[sublist])) :: tl
    in
    let make_up ~p items : Representation.element =
      if debug then eprintf "(OMD) make_up p=%b\n%!" p;
      let items = List.rev items in
      match items with
      | (U, _, _item) :: _ ->
          if p then
            Ulp (List.map (fun (_, _, i) -> i) items)
          else
            Ul (List.map (fun (_, _, i) -> i) items)
      | (O, _, _item) :: _ ->
          if p then
            Olp (List.map (fun (_, _, i) -> i) items)
          else
            Ol (List.map (fun (_, _, i) -> i) items)
      | [] ->
          failwith "make_up called with []" (* assert false *)
    in
    let rec list_items ~p indents items l =
      if debug then
        eprintf "(OMD) list_items: p=%b l=(%s)\n%!"
          p (L.destring_of_tokens l);
      match l with
      (* no more list items *)
      | [] ->
          make_up ~p items, l
      (* more list items *)
      (* new unordered items *)
      | Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: tl ->
          begin match fsplit_rev ~f:(end_of_item 0) tl with
          | None ->
              make_up ~p items, l
          | Some (new_item, rest) ->
              let p = p || has_paragraphs new_item in
              if debug then
                eprintf "(OMD) (2346) new_item=%S\n%!"
                  (L.destring_of_tokens new_item);
              match indents with
              | [] ->
                  assert(items = []);
                  list_items ~p [0] ((U, [0], rev_to_t new_item) :: items) rest
              | 0::_ ->
                  list_items ~p indents ((U, indents, rev_to_t new_item) :: items) rest
              | _::_ ->
                  make_up ~p items, l
          end
      | Delim (1, Space) :: Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: tl ->
          begin match fsplit_rev ~f:(end_of_item 1) tl with
          | None ->
              make_up ~p items, l
          | Some (new_item, rest) ->
              let p = p || has_paragraphs new_item in
              match indents with
              | [] ->
                  assert(items = []);
                  list_items ~p [1] ((U, [1], rev_to_t new_item) :: items) rest
              | 1::_ ->
                  list_items ~p indents ((U, indents, rev_to_t new_item) :: items) rest
              | i::_ ->
                  if i > 1 then
                    make_up ~p items, l
                  else (* i < 1 : new sub list*)
                    let sublist, remains =
                      list_items ~p (1::indents)
                        [(U, 1 :: indents, rev_to_t new_item)] rest
                    in
                    list_items ~p indents (add sublist items) remains
          end
      | Delim (n, Space) :: Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: tl when n >= 2 ->
          begin match fsplit_rev ~f:(end_of_item (n+2)) tl with
          | None ->
              make_up ~p items, l
          | Some(new_item, rest) ->
              let p = p || has_paragraphs new_item in
              match indents with
              | [] ->
                  if debug then
                    eprintf "(OMD) spaces[] l=(%S)\n%!" (L.string_of_tokens l);
                  assert(items = []); (* aïe... listes mal formées ?! *)
                  list_items ~p [n+2] ((U, [n+2], rev_to_t new_item) :: items) rest
              | i::_ ->
                  if debug then eprintf "(OMD) spaces(%d::_) n=%d l=(%S)\n%!"
                      i n (L.string_of_tokens l);
                  if i = n + 2 then
                    let items = (U,indents, rev_to_t new_item) :: items in
                    list_items ~p indents items rest
                  else if i < n + 2 then
                    let sublist, remains =
                      list_items ~p ((n+2)::indents)
                        [(U, (n+2) :: indents, rev_to_t new_item)]
                        rest
                    in
                    list_items ~p indents (add sublist items) remains
                  else (* i > n + 2 *)
                    make_up ~p items, l
          end
      (* new ordered items *)
      | Number _ :: Delim (1, Dot) :: Delim (_, Space) :: tl ->
          begin match fsplit_rev ~f:(end_of_item 0) tl with
          | None ->
              make_up ~p items, l
          | Some(new_item, rest) ->
              let p = p || has_paragraphs new_item in
              assert_well_formed new_item;
              match indents with
              | [] ->
                  assert (items = []);
                  list_items ~p [0] ((O, [0], rev_to_t new_item) :: items) rest
              | 0::_ ->
                  list_items ~p indents ((O, indents, rev_to_t new_item) :: items) rest
              | _::_ ->
                  make_up ~p items, l
          end
      | Delim (1, Space) :: Number _ :: Delim (1, Dot) :: Delim (_, Space) :: tl ->
          begin match fsplit_rev ~f:(end_of_item 1) tl with
          | None ->
              make_up ~p items, l
          | Some (new_item, rest) ->
              let p = p || has_paragraphs new_item in
              match indents with
              | [] ->
                  assert(items = []);
                  list_items ~p [1] ((O, [1], rev_to_t new_item) :: items) rest
              | 1::_ ->
                  list_items ~p indents ((O, indents, rev_to_t new_item) :: items) rest
              | i::_ ->
                  if i > 1 then
                    make_up ~p items, l
                  else (* i < 1 : new sub list*)
                    let sublist, remains =
                      list_items ~p (1::indents)
                        [(O,1 :: indents, rev_to_t new_item)] rest
                    in
                    list_items ~p:p indents (add sublist items) remains
          end
      | Delim (n, Space) :: Number _ :: Delim (1, Dot) :: Delim (_, Space) :: tl when n >= 2 ->
          begin match fsplit_rev ~f:(end_of_item (n+2)) tl with
          | None ->
              make_up ~p items, l
          | Some(new_item, rest) ->
              let p = p || has_paragraphs new_item in
              match indents with
              | [] ->
                  if debug then eprintf "(OMD) spaces[] l=(%S)\n%!"
                      (L.string_of_tokens l);
                  assert(items = []); (* aïe... listes mal formées ?! *)
                  list_items ~p [n+2] ((O, [n+2], rev_to_t new_item) :: items) rest
              | i::_ ->
                  if debug then eprintf "(OMD) spaces(%d::_) n=%d l=(%S)\n%!"
                      i n (L.string_of_tokens l);
                  if i = n + 2 then
                    list_items ~p indents ((O, indents, rev_to_t new_item) :: items) rest
                  else if i < n + 2 then
                    let sublist, remains =
                      list_items ~p
                        ((n+2) :: indents)
                        [(O, (n+2) :: indents, rev_to_t new_item)]
                        rest
                    in
                    list_items ~p:p indents (add sublist items) remains
                  else (* i > n + 2 *)
                    make_up ~p items, l
          end
      | Delim (2, Newline) :: (Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: _ as l)
      | Delim (2, Newline) :: (Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _ as l)
      | Delim (2, Newline) :: (Delim (_, Space) :: Delim (1, Star) :: Delim (_, Space) :: _ as l)
      | Delim (2, Newline) :: (Delim (_, Space) :: Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _ as l) ->
          list_items ~p:true indents items l
      | _ ->
          if debug then begin
            let rec string_of_items items =
              match items with
              | [] -> ""
              | (O, indent :: _, item) :: tl ->
                  sprintf "(O,i=%d,%S)" (indent) (Backend.html_of_md item) ^ string_of_items tl
              | (U, indent :: _, item) :: tl ->
                  sprintf "(U,i=%d,%S)" (indent) (Backend.html_of_md item) ^ string_of_items tl
              | _ ->
                  "(weird)"
            in
            eprintf "(OMD) NALI parse_list: l=(%S) items=%s\n%!"
              (L.string_of_tokens l) (string_of_items items)
          end;
          (* not a list item *)
          make_up ~p items, l
    in
    let rp, l = list_items ~p:false [] [] l in
    rp::r, [Delim (1, Newline)], l

  let icode ?(default_lang = default_lang) r _p l =
    assert_well_formed l;
    (* indented code: returns (r,p,l) where r is the result, p is the
       last thing read, l is the remains *)
    let dummy_tag =
      Tag("dummy_tag",
          object
            method to_string = ""
            method parser_extension = fun _r _p _l -> None
          end)
    in
    let accu = Buffer.create 64 in
    let rec loop s tl =
      match s, tl with
      | Delim (_, Newline) as p, Delim ((1|2|3), Space) :: _ ->
          (* 1, 2 or 3 spaces. *)
          (* -> Return what's been found as code because what follows isn't. *)
          Code_block (default_lang, Buffer.contents accu) :: r, [p], tl
      | Delim (_, Newline) as p, Delim (n, Space) :: tl when n >= 4 ->
          (* At least 4 spaces, it's still code. *)
          Buffer.add_string accu (L.string_of_token p);
          loop (if n >= 5 then Delim (n-4, Space) else dummy_tag) tl
      | Delim (_, Newline) as p, (_not_spaces :: _ as tl) -> (* stop *)
          Code_block (default_lang, Buffer.contents accu) :: r, [p], tl
      (* -> Return what's been found as code because it's no more code. *)
      | p, e :: tl ->
          Buffer.add_string accu (L.string_of_token p);
          (* html entities are to be converted later! *)
          loop e tl
      | p, [] ->
          Buffer.add_string accu (L.string_of_token p);
          Code_block (default_lang, Buffer.contents accu) :: r, [p], []
    in
    match l with
    | Delim (n, Space) :: tl when n >= 2 ->
        if n >= 5 then
          Some (loop (Delim (n-4, Space)) tl)
        else
          Some (loop dummy_tag tl)
    | _ ->
        assert false

  (* Returns [(r,p,l)] where [r] is the result, [p] is the last thing
     read, and [l] is what remains. *)
  let spaces_at_beginning_of_line main_loop default_lang n r previous lexemes =
    assert_well_formed lexemes;
    assert (n > 0);
    if n <= 3 then begin
      match lexemes with
      | Delim (1, (Star|Minus|Plus)) :: Delim (_, Space) :: _ ->
          (* unordered list *)
          parse_list main_loop r [] (L.make_space n :: lexemes)
      | Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _ ->
          (* ordered list *)
          parse_list main_loop r [] (L.make_space n :: lexemes)
      | [] | Delim (_, Newline) :: _  -> (* blank line, skip spaces *)
          r, previous, lexemes
      | _ :: _ ->
          Text (" ") :: r, previous, lexemes
    end else begin (* n>=4, blank line or indented code *)
      match lexemes with
      | [] | Delim (_, Newline) :: _  ->
          r, previous, lexemes
      | _ ->
          begin match icode ~default_lang r [Delim (1, Newline)] (L.make_space n :: lexemes) with
          | Some (r, p, l) ->
              r, p, l
          | None ->
              if debug then eprintf "(OMD) Omd_parser.icode or Omd_parser.main_loop is broken\n%!";
              assert false
          end
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
                Some (Url ("mailto:" ^ email, [Text email], "") :: r, [Delim (1, Greaterthan)], tl)
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

  let filter_text_omd_rev l =
    let rec loop b r = function
      | [] -> if b then r else l
      | ("media:type", Some "text/omd")::tl ->
          loop true r tl
      | e::tl ->
          loop b (e::r) tl
    in
    loop false [] l

  exception Orphan_closing of string * l * l

  let rec main_impl_rev ~html (r:r) (previous:p) (lexemes:l) =
    (* if debug then eprintf "(OMD) main_impl_rev html=%b\n%!" html; *)
    assert_well_formed lexemes;
    if debug then
      eprintf "(OMD) main_impl_rev html=%b r=%s p=(%s) l=(%s)\n%!"
        html
        (Backend.sexpr_of_md (List.rev r))
        (L.destring_of_tokens previous)
        (L.destring_of_tokens lexemes);
    match previous, lexemes with
    (* no more to process *)
    | _, [] ->
        (* return the result (/!\ it has to be reversed as some point) *)
        r

    (* Tag: tag system $\cup$ high-priority extension mechanism *)
    | _, Tag (_name, e) :: tl ->
        begin match e#parser_extension r previous tl with
        | Some (r, p, l) ->
            main_impl_rev ~html r p l
        | None ->
            main_impl_rev ~html r previous tl
        end

    (* HTML comments *)
    | _, (Delim (1, Lessthan) as t) :: (Delim (1, Exclamation) :: Delim (2, Minus) :: _ as tl) ->
        let f = function
          | Delim (x, Minus) as m :: (Delim (_, Greaterthan) as g) :: tl when x >= 2 ->
              Split ([g; m], tl)
          | _ ->
              Continue
        in
        begin match fsplit ~f lexemes with
        | None ->
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t)::r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
        | Some (comments, new_tl) ->
            let r = Html_comment (L.string_of_tokens comments) :: r in
            main_impl_rev ~html r [Delim (1, Newline)] new_tl
        end

    (* email-style quoting / blockquote *)
    | ([] | [Delim (_, Newline)]), Delim (1, Greaterthan) :: Delim (_, Space) :: _ ->
        begin match emailstyle_quoting main_loop r previous (Delim (1, Newline) :: lexemes) with
        | Some (r,p,l) ->
            main_impl_rev ~html r p l
        | None ->
            if debug then
              eprintf "(OMD) Omd_parser.emailstyle_quoting or Omd_parser.main_loop is broken\n%!";
            assert false
        end

    (* email-style quoting, with lines starting with spaces! *)
    | ([] | [Delim (_, Newline)]), (Delim ((1|2|3), Space) as s) :: Delim (1, Greaterthan) :: Delim (_, Space) :: _ ->
        (* It's 1, 2 or 3 spaces, not more because it wouldn't mean
           quoting anymore but code. *)
        let new_r, _p, rest =
          let foo, rest =
            match unindent (L.length s) (Delim (1, Newline) :: lexemes) with
            | Delim (_, Newline) :: foo, rest ->
                foo, rest
            | res ->
                res
          in
          match emailstyle_quoting main_loop [] previous (Delim (1, Newline) :: foo) with
          | Some (new_r, p, []) ->
              new_r, p, rest
          | _ ->
              if debug then
                eprintf "(OMD) Omd_parser.emailstyle_quoting or Omd_parser.main_loop is broken\n%!";
              assert false
        in
        main_impl_rev ~html (new_r@r) [Delim (1, Newline)] rest

    (* minus *)
    | ([] | [Delim (_, Newline)]), (Delim (_, Minus) as t) :: (Delim (_, Space) :: _ as tl) ->
        (* maybe hr *)
        begin match hr_m lexemes with
        | None -> (* no hr, so it could be a list *)
            begin match t with
            | Delim (1, Minus) -> (* it's a list *)
                let md, new_p, new_l = parse_list main_loop r [] lexemes in
                main_impl_rev ~html md new_p new_l
            | _ -> (* not a list *)
                begin match maybe_extension extensions r previous lexemes with
                | None ->
                    main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
                | Some (r, p, l) ->
                    main_impl_rev ~html r p l
                end
            end
        | Some l -> (* hr *)
            main_impl_rev ~html (Hr :: r) [Delim (1, Newline)] l
        end
    | ([] | [Delim (_, Newline)]), (Delim (_, Minus) as t) :: tl ->
        begin match hr_m lexemes with
        | None -> (* no hr, and it's not a list either
                     because it's not followed by spaces *)
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t)::r) [t] tl
            | Some(r, p, l) ->
                main_impl_rev ~html r p l
            end
        | Some l -> (* hr *)
            main_impl_rev ~html (Hr :: r) [Delim (1, Newline)] l
        end

    (* hashes *)
    | ([] | [Delim (_, Newline)]), (Delim (n, Hash) as t) :: (Delim (_, Space) :: ttl as tl)
    | ([] | [Delim (_, Newline)]), (Delim (n, Hash) as t) :: (ttl as tl) -> (* hash titles *)
        if n <= 6 then
          match read_title main_loop n r previous ttl with
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
          | None ->
              if debug then
                eprintf "(OMD) Omd_parser.read_title or Omd_parser.main_loop is broken\n%!";
              assert false
        else begin
          match maybe_extension extensions r previous lexemes with
          | None ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
        end
    | _, (Delim (_, Hash) as t) :: tl -> (* hash -- no title *)
        begin match maybe_extension extensions r previous lexemes with
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        | Some (r, p, l) ->
            main_impl_rev ~html r p l
        end

    (* spaces after a newline: could lead to hr *)
    | ([] | [Delim (_, Newline)]), (Delim (_, Space) as sp) :: tl ->
        begin match hr tl with
        | None ->
            (* No [Hr], but maybe [Ul], [Ol], code,... *)
            let n = L.length sp in
            let r, p, l =
              spaces_at_beginning_of_line main_loop default_lang n r previous tl
            in
            main_impl_rev ~html r p l
        | Some tl ->
            main_impl_rev ~html (Hr :: r) [Delim (1, Newline)] tl
        end

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
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
        | Some (x, new_tl) ->
            main_impl_rev ~html (Emph (main_impl ~html [] [t] x) :: r) [t] new_tl
        end
    | _, (Delim ((2|3) as n, Underscore) as t) :: tl ->
        (* 2 or 3 "orphan" underscores, or emph/bold *)
        begin match uemph_or_bold n tl with
        | None ->
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
        | Some (x, new_tl) ->
            if n = 2 then (* 1 underscore *)
              main_impl_rev ~html (Bold (main_impl ~html [] [t] x) :: r) [t] new_tl
            else (* 2 underscores *)
              main_impl_rev ~html (Emph ([Bold (main_impl ~html [] [t] x)]) :: r) [t] new_tl
        end

    (* enumerated lists *)
    | ([] | [Delim (_, Newline)]), Number _ :: Delim (1, Dot) :: Delim (_, Space) :: _ ->
        let md, new_p, new_l = parse_list main_loop r [] lexemes in
        main_impl_rev ~html md new_p new_l

    (* plus *)
    | ([] | [Delim (_, Newline)]), Delim (1, Plus) :: Delim (_, Space) :: _ ->
        let md, new_p, new_l = parse_list main_loop r [] lexemes in
        main_impl_rev ~html md new_p new_l

    (* stars *)
    | ([] | [Delim (_, Newline)]), Delim (1, Star) :: Delim (_, Space) :: _ ->
        (* maybe hr or new list *)
        begin match hr_s lexemes with
        | Some l ->
            main_impl_rev ~html (Hr :: r) [Delim (1, Newline)] l
        | None ->
            let md, new_p, new_l = parse_list main_loop r [] lexemes in
            main_impl_rev ~html md new_p new_l
        end
    | ([] | [Delim (_, Newline)]), Delim (n, Star) :: _ when hr_s lexemes <> None && n >= 2 ->
        (* hr *)
        begin match hr_s lexemes with
        | Some l ->
            main_impl_rev ~html (Hr :: r) [Delim (1, Newline)] l
        | None ->
            assert false
        end
    | ([] | [Delim (_, Newline)]), (Delim (1, Star) as t) :: tl -> (* maybe hr *)
        begin match hr_s lexemes with
        | Some l ->
            main_impl_rev ~html (Hr :: r) [Delim (1, Newline)] l
        | None ->
            begin match semph_or_bold 1 tl with
            | Some(x, new_tl) ->
                main_impl_rev ~html (Emph (main_impl ~html [] [t] x) :: r) [t] new_tl
            | None ->
                begin match maybe_extension extensions r previous lexemes with
                | None ->
                    main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
                | Some (r, p, l) ->
                    main_impl_rev ~html r p l
                end
            end
        end
    | _, (Delim (1, Star) as t) :: tl -> (* one "orphan" star, or emph // can't be hr *)
        begin match semph_or_bold 1 tl with
        | Some(x, new_tl) ->
            main_impl_rev ~html (Emph (main_impl ~html [] [t] x) :: r) [t] new_tl
        | None ->
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
        end
    | _, (Delim ((2|3) as n, Star) as t) :: tl ->
        (* 2 or 3 "orphan" stars, or emph/bold *)
        begin match semph_or_bold n tl with
        | Some (x, new_tl) ->
            if n = 2 then
              main_impl_rev ~html (Bold (main_impl ~html [] [t] x) :: r) [t] new_tl
            else
              main_impl_rev ~html (Emph ([Bold (main_impl ~html [] [t] x)]) :: r) [t] new_tl
        | None ->
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
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
            main_impl_rev ~html (Url (url, [Text url],"") :: r) [] new_tl
        | None ->
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            | Some(r, p, l) ->
                main_impl_rev ~html r p l
            end
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
        begin match bcode ~default_lang r previous lexemes with
        | Some(r, p, l) -> main_impl_rev ~html r p l
        | None ->
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t)::r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
        end

    (* HTML *)
    (* <br/> and <hr/> with or without space(s) *)
    | _, Delim (1, Lessthan) :: Word ("br"|"hr" as w) :: Delim (1, Slash) :: Delim (n, Greaterthan) :: tl
    | _, Delim (1, Lessthan) :: Word ("br"|"hr" as w) :: Delim (_, Space) :: Delim (1, Slash) :: Delim (n, Greaterthan) :: tl ->
        main_impl_rev ~html (Raw("<"^w^" />")::r) [Delim (1, Greaterthan)] (delim (n-1) Greaterthan tl)

    (* awaited orphan html closing tag *)
    | _, Delim (1, Lessthan) :: Delim (1, Slash) :: Word w :: Delim (n, Greaterthan) :: tl when !mediatypetextomd <> [] ->
        raise (Orphan_closing(w, lexemes, delim (n-1) Greaterthan tl))

    (* block html *)
    | ([] | [Delim (_, Newline) | Tag ("HTMLBLOCK", _)]),
      (Delim (1, Lessthan) as t) ::
      ((Word(tagnametop) as w) :: ((Delim (_, Space) | Delim (_, Greaterthan)) :: _ as html_stuff) as tlx) ->
        if StringSet.mem tagnametop inline_htmltags_set then
          main_impl_rev ~html r [Word ""] lexemes
        else if not (blind_html || StringSet.mem tagnametop htmltags_set) then begin
          match maybe_extension extensions r previous lexemes with
          | None ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tlx
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
        end else
          let read_html () =
            let module T = struct
              type t =
                | Awaiting of string
                | Open of string

              type interm =
                | HTML of string * (string * string option) list * interm list
                | FTOKENS of L.t
                | RTOKENS of L.t
                | MD of Representation.t

              let rec md_of_interm_list html l =
                let md_of_interm_list ?(html = html) l = md_of_interm_list html l in
                match l with
                | [] ->
                    []
                | HTML (t, a, c) :: tl ->
                    let f_a = filter_text_omd_rev a in
                    if f_a != a then
                      Html_block (t, f_a, make_paragraphs (md_of_interm_list ~html:false (List.rev c))) ::
                      md_of_interm_list tl
                    else
                      Html_block (t, f_a, md_of_interm_list ~html:true (List.rev c)) ::
                      md_of_interm_list tl
                | MD md :: tl ->
                    md@md_of_interm_list tl
                | RTOKENS t1 :: FTOKENS t2 :: tl ->
                    md_of_interm_list (FTOKENS (List.rev_append t1 t2) :: tl)
                | RTOKENS t1 :: RTOKENS t2 :: tl ->
                    md_of_interm_list (FTOKENS (List.rev_append t1 (List.rev t2)) :: tl)
                | FTOKENS t1 :: FTOKENS t2 :: tl ->
                    md_of_interm_list (FTOKENS (t1@t2) :: tl)
                | FTOKENS t :: tl ->
                    if html then
                      Raw(L.string_of_tokens t) :: md_of_interm_list tl
                    else
                      main_loop ~html [] [Word ""] t @ md_of_interm_list tl
                | RTOKENS t :: tl ->
                    md_of_interm_list (FTOKENS (List.rev t) :: tl)

              let md_of_interm_list l = md_of_interm_list true l

              let string_of_tagstatus tagstatus =
                let b = Buffer.create 64 in
                List.iter (function
                    | Open t -> bprintf b "{B/Open %s}" t
                    | Awaiting t -> bprintf b "{B/Awaiting %s}" t
                  ) tagstatus;
                Buffer.contents b
            end
            in
            let add_token_to_body x body =
              match body with
              | T.RTOKENS r :: body ->
                  T.RTOKENS(x::r)::body
              | _ ->
                  T.RTOKENS [x] :: body
            in
            let rec loop (body:T.interm list) attrs tagstatus tokens =
              if debug then
                eprintf "(OMD) 3333 BHTML loop body=%S tagstatus=%S %S\n%!"
                  (Backend.sexpr_of_md(T.md_of_interm_list body))
                  (T.string_of_tagstatus tagstatus) (L.destring_of_tokens tokens);
              match tokens with
              | [] ->
                  begin match tagstatus with
                  | [] ->
                      Some(body, tokens)
                  | T.Open t :: _ when StringSet.mem t html_void_elements ->
                      Some(body, tokens)
                  | _ ->
                      if debug then eprintf "(OMD) 3401 BHTML Not enough to read\n%!";
                      None
                  end
              | Delim (n, Lessthan) :: tokens when n >= 2 ->
                  begin match tagstatus with
                  | T.Awaiting _ :: _ ->
                      None
                  | _ ->
                      if debug then eprintf "(OMD) 3408 BHTML loop\n%!";
                      loop (add_token_to_body (Delim (n-1, Lessthan)) body)
                        attrs tagstatus (Delim (1, Lessthan) :: tokens)
                  end
              (* self-closing tags *)
              | Delim (1, Slash) :: Delim (1, Greaterthan) :: tokens ->
                  begin match tagstatus with
                  | T.Awaiting(tagname) :: tagstatus when StringSet.mem tagname html_void_elements ->
                      loop [T.HTML (tagname, attrs, [])] [] tagstatus tokens
                  | _ ->
                      if debug then eprintf "(OMD) 3419 BHTML loop\n%!";
                      loop (add_token_to_body (Delim (1, Slash)) (add_token_to_body (Delim (1, Greaterthan)) body))
                        attrs tagstatus tokens
                  end
              (* closing the tag opener *)
              | Delim (1, Lessthan) :: Delim (1, Slash) :: (Word tagname as w) :: (Delim (n, Greaterthan) as g) :: tokens ->
                  begin match tagstatus with
                  | T.Open t :: _ when t = tagname ->
                      if debug then eprintf "(OMD) 3375 BHTML properly closing %S\n%!" t;
                      Some (body, delim (n-1) Greaterthan tokens)
                  | T.Open t :: _ ->
                      if debug then eprintf "(OMD) 3379 BHTML wrongly closing %S with %S 1\n%!" t tagname;
                      loop (T.RTOKENS [g; w; Delim (1, Slash); Delim (1, Lessthan)] :: body) [] tagstatus tokens
                  | T.Awaiting t :: _ ->
                      if debug then eprintf "(OMD) 3383 BHTML wrongly closing %S with %S 2\n%!" t tagname;
                      if !mediatypetextomd <> [] then
                        raise (Orphan_closing(t, lexemes, delim (n-1) Greaterthan tokens))
                      else
                        None
                  | [] ->
                      if debug then eprintf "(OMD) BHTML wrongly closing %S 3\n%!" tagname;
                      None
                  end
              (* tag *)
              | Delim (1, Lessthan) :: (Word tagname as word) :: tokens when blind_html || StringSet.mem tagname htmltags_set ->
                  if debug then eprintf "(OMD) 3489 BHTML <Word(%s)...\n%!" tagname;
                  begin match tagstatus with
                  | T.Open t :: _ when t <> tagname && StringSet.mem t html_void_elements ->
                      None
                  | T.Awaiting _ :: _ ->
                      None
                  | _ ->
                      if attrs <> [] then begin
                        if debug then eprintf "(OMD) 3496 BHTML tag %S but attrs <> []\n%!" tagname;
                        None
                      end else begin
                        if debug then
                          eprintf "(OMD) 3421 BHTML tag %S, tagstatus=%S, attrs=[], tokens=%S\n%!"
                            tagname (T.string_of_tagstatus tagstatus) (L.destring_of_tokens tokens);
                        match loop [] [] (T.Awaiting tagname::tagstatus) tokens with
                        | None ->
                            if debug then eprintf "(OMD) 3489 BHTML loop\n%!";
                            loop (add_token_to_body word (add_token_to_body (Delim (1, Lessthan)) body))
                              attrs tagstatus tokens
                        | Some (b, tokens) ->
                            if debug then
                              eprintf "(OMD) 3433 BHTML tagstatus=%S tokens=%S\n%!"
                                (T.string_of_tagstatus tagstatus) (L.string_of_tokens tokens);
                            Some (b@body, tokens)
                      end
                  end
              (* end of opening tag *)
              | Delim (1, Greaterthan) :: tokens ->
                  begin match tagstatus with
                  | T.Awaiting t :: tagstatus ->
                      if List.mem ("media:type", Some "text/omd") attrs then begin
                        mediatypetextomd := t :: !mediatypetextomd;
                        try
                          ignore(main_impl_rev ~html [] [] tokens);
                          if debug then
                            eprintf "(OMD) 3524 BHTML closing tag not found in %S\n%!" (L.destring_of_tokens tokens);
                          warn (sprintf "Closing tag `%s' not found for text/omd zone." t);
                          mediatypetextomd := List.tl !mediatypetextomd;
                          None
                        with Orphan_closing (tagname, delimiter, after) ->
                          let before =
                            let rec f r = function
                              | Delim (n, Lessthan) as e :: tl when n >= 2 ->
                                  begin match delimiter with
                                  | Delim (1, Lessthan) :: _ ->
                                      if Delim (1, Lessthan) :: tl = delimiter then
                                        List.rev (delim (n-1) Lessthan r)
                                      else
                                        f (e :: r) tl
                                  | _ ->
                                      if tl = delimiter then
                                        List.rev r
                                      else
                                        f (e :: r) tl
                                  end
                              | e :: tl as l ->
                                  if l = delimiter then
                                    List.rev r
                                  else if tl = delimiter then
                                    List.rev (e :: r)
                                  else
                                    f (e :: r) tl
                              | [] ->
                                  List.rev r
                            in
                            f [] tokens
                          in
                          if debug then
                            eprintf "(OMD) 3552 BHTML tokens=%s delimiter=%s \
                                     after=%s before=%s (tagname=t)=%b\n%!"
                              (L.destring_of_tokens tokens)
                              (L.destring_of_tokens delimiter)
                              (L.destring_of_tokens after)
                              (L.destring_of_tokens before)
                              (tagname = t);
                          begin match !mediatypetextomd with
                          | _ :: tl ->
                              mediatypetextomd := tl
                          | [] ->
                              assert false
                          end;
                          if tagname = t then
                            loop
                              [T.HTML (t, attrs, [T.MD (main_impl ~html [] [] (tag_setext main_loop before))])]
                              [] tagstatus after
                          else
                            None
                      end else begin
                        if debug then eprintf "(OMD) 3571 BHTML loop\n%!";
                        match loop body [] (T.Open t :: tagstatus) tokens with
                        | None ->
                            if debug then
                              eprintf "(OMD) 3519 BHTML Couldn't find an closing tag for %S\n%!" t;
                            None
                        | Some(body, l) ->
                            if debug then
                              eprintf "(OMD) 3498 BHTML Found a closing tag %s\n%!" t;
                            match tagstatus with
                            | _ :: _ ->
                                loop [T.HTML (t, attrs, body)] [] tagstatus l
                            | [] ->
                                Some([T.HTML (t, attrs, body)], l)
                      end
                  | T.Open _ :: _ ->
                      if debug then eprintf "(OMD) 3591 BHTML Some `>` isn't for an opening tag\n%!";
                      loop (add_token_to_body (Delim (1, Greaterthan)) body) attrs tagstatus tokens
                  | [] ->
                      if debug then eprintf "(OMD) 3542 BHTML tagstatus=[]\n%!";
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
                    | Word _ | Number _ | Delim (_, (Colon|Underscore|Minus|Dot)) as t :: tokens ->
                        extract_attribute (t::accu) tokens
                    | tokens ->
                        Void, tokens
                  in
                  begin match extract_attribute [t] tokens with
                  | Empty attributename, tokens ->
                      (* attribute with no explicit value *)
                      if debug then eprintf "(OMD) 3628 BHTML loop\n%!";
                      loop body ((attributename, None) :: attrs) tagstatus tokens
                  | Named attributename, tokens ->
                      begin match tokens with
                      | Delim (2, Quote) :: tokens ->
                          if debug then
                            eprintf "(OMD) 3661 BHTML empty attribute 1 %S\n%!" (L.string_of_tokens tokens);
                          loop body ((attributename, Some "") :: attrs) tagstatus tokens
                      | Delim (1, Quote) :: tokens ->
                          if debug then
                            eprintf "(OMD) 3668 BHTML non empty attribute 1 %S\n%!" (L.string_of_tokens tokens);
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
                                )
                              tokens
                          with
                          | None ->
                              None
                          | Some(at_val, tokens) ->
                              if debug then eprintf "(OMD) 3654 BHTML loop\n%!";
                              loop body ((attributename, Some (L.string_of_tokens at_val)) :: attrs) tagstatus tokens
                          end
                      | Delim (2, Doublequote) :: tokens ->
                          if debug then
                            eprintf "(OMD) 3690 BHTML empty attribute 2 %S\n%!" (L.string_of_tokens tokens);
                          loop body ((attributename, Some "") :: attrs) tagstatus tokens
                      | Delim (1, Doublequote) :: tokens ->
                          if debug then
                            eprintf "(OMD) 3698 BHTML non empty attribute 2 %S\n%!"
                              (L.string_of_tokens tokens);
                          begin match
                            fsplit
                              ~excl:(function
                                  | Delim (n, Doublequote) :: _ when n >= 2 -> true
                                  | _ -> false
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
                                eprintf "(OMD) 3622 BHTML %s=%S %s\n%!" attributename
                                  (L.string_of_tokens at_val) (L.destring_of_tokens tokens);
                              loop body ((attributename, Some (L.string_of_tokens at_val)) :: attrs)
                                tagstatus tokens
                          end
                      | _ ->
                          None
                      end
                  | Void, _ ->
                      None
                  end

              | x :: tokens as dgts when (match tagstatus with T.Open _ :: _ -> true | _ -> false) ->
                  if debug then
                    eprintf "(OMD) 3620 BHTML general %S\n%!"
                      (L.string_of_tokens dgts);
                  loop (add_token_to_body x body) attrs tagstatus tokens
              | (Delim (1, Newline) | Delim (_, Space)) :: tokens when (match tagstatus with T.Awaiting _ :: _ -> true | _ -> false) ->
                  if debug then eprintf "(OMD) 3737 BHTML spaces\n%!";
                  loop body attrs tagstatus tokens
              | (Delim (n, Newline) as x) :: tokens when n >= 2 && (match tagstatus with T.Awaiting _ :: _ -> true | _ -> false) ->
                  if debug then eprintf "(OMD) 3827 BHTML newlines\n%!";
                  warn "there are empty lines in what may be an HTML block";
                  loop (add_token_to_body x body) attrs tagstatus tokens
              | _ ->
                  if debug then
                    eprintf "(OMD) 3742 BHTML fallback with tokens=%s and tagstatus=%s\n%!"
                      (L.destring_of_tokens tokens)
                      (match tagstatus with
                       | [] -> "None"
                       | T.Awaiting _ :: _ -> "Awaiting"
                       | T.Open _ :: _ -> "Open (can't be)");
                  begin match tagstatus with
                  | [] ->
                      Some(body, tokens)
                   | T.Awaiting tag :: _ ->
                       warn (sprintf "expected to read an open HTML tag (%s), but found nothing" tag);
                       None
                   | T.Open tag :: _ ->
                       warn (sprintf "expected to find the closing HTML tag for %s, but found nothing" tag);
                       None
                  end
            in
            if debug then eprintf "(OMD) 3408 BHTML loop\n%!";
            match loop [] [] [] lexemes with
            | Some (h, rest) ->
                Some (T.md_of_interm_list h, rest)
            | None ->
                None
          in
          begin match read_html() with
          | Some(h, rest) ->
              main_impl_rev ~html (h@r) [Tag ("HTMLBLOCK", empty_extension)] rest
          | None ->
              let text = L.string_of_token t in
              main_impl_rev ~html (Text (text ^ tagnametop)::r) [w] html_stuff
          end
    (* / end of block HTML. *)

    (* inline HTML *)
    | _, (Delim (1, Lessthan) as t) :: ((Word tagnametop as w) :: (Delim (_, (Space|Greaterthan)) :: _ as html_stuff) as tlx) ->
        if (strict_html && not(StringSet.mem tagnametop inline_htmltags_set)) ||
           not (blind_html || StringSet.mem tagnametop htmltags_set)
        then begin
          match maybe_extension extensions r previous lexemes with
          | None ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tlx
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
        end else
          let read_html() =
            let module T = struct
              type t =
                | Awaiting of string
                | Open of string

              type interm =
                | HTML of string * (string * string option) list * interm list
                | TOKENS of L.t
                | MD of Representation.t

              let rec md_of_interm_list = function
                | [] ->
                    []
                | HTML (t, a, c) :: tl ->
                    Html (t, a, md_of_interm_list (List.rev c)) :: md_of_interm_list tl
                | MD md :: tl ->
                    md @ md_of_interm_list tl
                | TOKENS t1 :: TOKENS t2 :: tl ->
                    md_of_interm_list (TOKENS (t1@t2) :: tl)
                | TOKENS t :: tl ->
                    main_impl ~html [] [Word ""] (t) @ md_of_interm_list tl

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
                      | Some ((Code _ :: _) as c, _, l) ->
                          if debug then eprintf "(OMD) maybe code in inline HTML: confirmed\n%!";
                          loop (T.MD c :: body) [] tagstatus l
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
            begin match maybe_extension extensions r previous lexemes with
            | None ->
                main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
            | Some (r, p, l) ->
                main_impl_rev ~html r p l
            end
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
            begin match maybe_reference main_loop rc r previous tl with
            | Some(r, p, l) ->
                main_impl_rev ~html r p l
            | None ->
                begin match maybe_extension extensions r previous lexemes with
                | None ->
                    main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
                | Some (r, p, l) ->
                    main_impl_rev ~html r p l
                end
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
          begin match maybe_extension extensions r previous lexemes with
          | None ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
          end
        end

    (* img ref *)
    | _, (Delim (1, Exclamation) as t) :: Delim (1, Obracket) :: Delim (1, Cbracket) :: Delim (1, Obracket) :: tl ->
        (* ref image insertion with no "alt" *)
        (* ![][ref] *)
        begin try
          let id, tl = read_until_cbracket ~bq:true ~no_nl:true tl in
          let fallback = extract_fallback main_loop tl lexemes in
          let id = L.string_of_tokens id in
          main_impl_rev ~html (Img_ref (rc, id, "", fallback) :: r) [Delim (1, Cbracket)] tl
        with NL_exception ->
          begin match maybe_extension extensions r previous lexemes with
          | None ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
          end
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
                begin match maybe_extension extensions r previous lexemes with
                | None ->
                    main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
                | Some(r, p, l) ->
                    main_impl_rev ~html r p l
                end
              end
          | alt, Delim (1, Obracket) :: Word id :: Delim (1, Cbracket) :: ntl
          | alt, Delim (1, Obracket) :: Delim (_, Space) :: Word id :: Delim (1, Cbracket) :: ntl
          | alt, Delim (1, Obracket) :: Delim (_, Space) :: Word id :: Delim (_, Space) :: Delim (1, Cbracket) :: ntl
          | alt, Delim (1, Obracket) :: Word id :: Delim (_, Space) :: Delim (1, Cbracket) :: ntl ->
              let fallback = extract_fallback main_loop ntl lexemes in
              let alt = L.string_of_tokens alt in
              main_impl_rev ~html (Img_ref(rc, id, alt, fallback) :: r) [Delim (1, Cbracket)] ntl
          | alt, Delim (1, Obracket) :: ((Delim (1, Newline) | Delim (_, Space) | Word _ | Number _) :: _ as ntl) ->
              begin try
                match read_until_cbracket ~bq:true ~no_nl:false ntl with
                | [], _rest ->
                    raise Premature_ending
                | id, rest ->
                    let fallback = extract_fallback main_loop rest lexemes in
                    let id = L.string_of_tokens id in
                    let alt = L.string_of_tokens alt in
                    main_impl_rev ~html (Img_ref (rc, id, alt, fallback)::r) [Delim (1, Cbracket)] rest
              with Premature_ending | NL_exception ->
                begin match maybe_extension extensions r previous lexemes with
                | None ->
                    main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
                | Some (r, p, l) ->
                    main_impl_rev ~html r p l
                end
              end
          | _ ->
              begin match maybe_extension extensions r previous lexemes with
              | None ->
                  main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
              | Some (r, p, l) ->
                  main_impl_rev ~html r p l
              end
        with Premature_ending ->
          begin match maybe_extension extensions r previous lexemes with
          | None ->
              main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
          | Some (r, p, l) ->
              main_impl_rev ~html r p l
          end
        end

    | _,
      (Delim (1, (At|Bar|Caret|Cbrace|Colon|Comma|Cparenthesis|Cbracket|Dollar
                 |Dot|Doublequote|Exclamation|Equal|Minus|Obrace|Oparenthesis
                 |Percent|Plus|Question|Quote|Semicolon|Slash|Tab|Tilde
                 |Greaterthan)) as t) :: tl ->
        begin match maybe_extension extensions r previous lexemes with
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        | Some (r, p, l) ->
            main_impl_rev ~html r p l
        end
    | _, (Number _  as t):: tl ->
        begin match maybe_extension extensions r previous lexemes with
        | None ->
            main_impl_rev ~html (Text (L.string_of_token t) :: r) [t] tl
        | Some (r, p, l) ->
            main_impl_rev ~html r p l
        end

    | _, (Delim (_, (At|Bar|Caret|Cbrace|Cbracket|Colon
                    |Comma|Cparenthesis|Dollar|Dot|Doublequote
                    |Equal|Exclamation|Greaterthan|Lessthan
                    |Minus|Obrace|Obracket|Oparenthesis
                    |Percent|Plus|Question|Quote|Semicolon
                    |Slash|Star|Tab|Tilde|Underscore)) as tk) :: tl ->
        begin match maybe_extension extensions r previous lexemes with
        | None ->
            let tk0, tks = L.split_first tk in
            let text = L.string_of_token tk0 in
            main_impl_rev ~html (Text text :: r) [tk0] (tks :: tl)
        | Some (r, p, l) ->
            main_impl_rev ~html r p l
        end

  and main_impl ~html (r:r) (previous:p) (lexemes:l) =
    (* if debug then eprintf "(OMD) main_impl html=%b\n%!" html; *)
    assert_well_formed lexemes;
    List.rev (main_loop_rev ~html r previous lexemes)

  and main_loop ?(html = false) (r:r) (previous:p) (lexemes:l) =
    main_impl ~html r previous lexemes

  and main_loop_rev ?(html = false) (r:r) (previous:p) (lexemes:l) =
    main_impl_rev ~html r previous lexemes

  let main_parse lexemes =
    main_loop [] [] (tag_setext main_loop lexemes)

  let parse lexemes =
    main_parse lexemes
end

let default_parse ?extensions:(e = []) ?default_lang:(d = "") lexemes =
  let module E = struct
    include Default_env (Unit)
    let extensions = e
    let default_lang = d
  end
  in
  let module M = Make(E) in
  M.main_parse lexemes

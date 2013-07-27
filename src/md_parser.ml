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
  
open Md
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

(* The few following lines are there for development purpose only. *)
let xspaces = ref None
let xnew_list = ref None
let xicode = ref None
let xmain_loop = ref None

exception Not_yet_implemented of Md_lexer.t list

let parse lexemes =
  let rec main_loop (r:md) (previous:Md_lexer.t list) (lexemes:Md_lexer.t list) =
    if debug then Printf.eprintf "main_loop p=(%s) l=(%s)\n%!" (destring_of_tl previous) (destring_of_tl lexemes);
    match previous, lexemes with

      (* no more to process *)
      | _, [] -> (* return the result (/!\ it has to be reversed as some point) *)
          r

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

      (* spaces *)
      | _, ((Space|Spaces _) as t) :: tl -> (* too many cases to be handled here *)
          let r, p, l = spaces (fst (length t)) r previous tl in
            main_loop r p l

      (* stars *)
      | ([]|[(Newline|Newlines _)]), Star :: (Space|Spaces _) :: _ -> (* new list *)
          begin match new_list r [] (Newline::lexemes) with
            | md, new_p, new_l -> main_loop (md@r) new_p new_l
          end          
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

      | _, (Word("http"|"https"|"ftp"|"ftps"|"ssh"|"afp"|"imap") as w)::Colon::Slashs(n)::tl -> (* automatic URLs *)
          let rec read_url accu = function
            | (Space|Spaces _|Tab|Tabs _|Newline|Newlines _|Return|Returns _)::tl ->
                (string_of_t w) ^ "://" ^ (if n = 0 then "" else String.make (n-2) '/') ^ string_of_tl (List.rev accu), tl
            | x::tl ->
                read_url (x::accu) tl
            | [] -> string_of_tl (List.rev accu), []
          in
          let url, new_tl = read_url [] tl in
            main_loop (Url url :: r) [] new_tl


      (* email addresses are not so simple to handle because of the possible presence of characters such as - _ + . *)
      (*       | _, ((Word _|Number _) as w1)::At::((Word _|Number _) as w2)::Dot::Word w3::tl -> *)
      (*           main_loop (Text (w1^"@"^w2) :: r) [] tl *)
      (*       | _, Word w1::At::Word w2::tl -> *)
      (* main_loop (Text (w1^"@"^w2) :: r) [] tl *)

      | _, Word w::tl ->
          main_loop (Text w :: r) [] tl
      | _, [Newline] ->
          Text "\n"::r
      | _, Ampersand::((Word w::((Semicolon|Semicolons _) as s)::tl) as tl2) ->
          let htmlentities = StringSet.of_list (* This list should be checked...*)
            ["ecirc"; "oacute"; "plusmn"; "para"; "sup"; "iquest"; "frac"; "aelig"; "ntilde";
             "Ecirc"; "Oacute"; "iexcl"; "brvbar"; "pound"; "not"; "macr"; "AElig"; "Ntilde"] in
            if StringSet.mem w htmlentities then
              begin match s with
                | Semicolon ->
                    main_loop (Text("&"^w^";")::r) [s] tl
                | Semicolons 0 ->
                    main_loop (Text("&"^w^";")::r) [s] (Semicolon::tl)
                | Semicolons n ->
                    main_loop (Text("&"^w^";")::r) [s] (Semicolons(n-1)::tl)
                | _ -> assert false
              end
            else
              main_loop (Text("&amp;")::r) [] tl2
      | _, Ampersand::tl ->
          main_loop (Text("&amp;")::r) [Ampersand] tl
      | _, Ampersands(0)::tl ->
          main_loop (Text("&amp;")::r) [] (Ampersand::tl)
      | _, Ampersands(n)::tl ->
          main_loop (Text("&amp;")::r) [] (Ampersands(n-1)::tl)
      | _, (Backquote|Backquotes _)::_ ->
          begin match bcode r previous lexemes with
            | r, p, l -> main_loop r p l
          end
      | _, (Lessthan|Lessthans _ as opening)::
          Word("a"|"abbr"|"acronym"|"address"|"applet"|"area"|"article"|"aside"
          |"audio"|"b"|"base"|"basefont"|"bdi"|"bdo"|"big"|"blockquote" (* |"body" *)
          |"br"|"button"|"canvas"|"caption"|"center"|"cite"|"code"|"col"
          |"colgroup"|"command"|"datalist"|"dd"|"del"|"details"|"dfn"|"dialog"
          |"dir"|"div"|"dl"|"dt"|"em"|"embed"|"fieldset"|"figcaption"|"figure"
          |"font"|"footer"|"form"|"frame"|"frameset"|"h1" (* |"head" *) |"header"|"hr"
                (* |"html" *) |"i"|"iframe"|"img"|"input"|"ins"|"kbd"|"keygen"|"label"
          |"legend"|"li"|"link"|"map"|"mark"|"menu" (* |"meta" *) |"meter"|"nav"
          |"noframes"|"noscript"|"object"|"ol"|"optgroup"|"option"|"output"|"p"
          |"param"|"pre"|"progress"|"q"|"rp"|"rt"|"ruby"|"s"|"samp"|"script"
          |"section"|"select"|"small"|"source"|"span"|"strike"|"strong"|"style"
          |"sub"|"summary"|"sup"|"table"|"tbody"|"td"|"textarea"|"tfoot"|"th"
          |"thead"|"time"|"title"|"tr"|"track"|"tt"|"u"|"ul"|"var"|"video"|"wbr" as tagname)::((Space|Spaces _|Greaterthan|Greaterthans _) as x)::tl ->
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
                  accu, []
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
      | _, Newline::tl ->
          main_loop (NL::r) [Newline] tl
      | _, Newlines _::tl ->
          main_loop (NL::NL::r) [Newline] tl
      | _,
          ((At|Ats _|Bar|Bars _|Caret
          |Carets _|Cbrace|Cbraces _|Colon|Colons _|Comma|Commas _|Cparenthesis|Cparenthesiss _
          |Cbracket|Cbrackets _|Dollar|Dollars _|Dot|Dots _|Doublequote|Doublequotes _
          |Exclamation|Exclamations _|Equal|Equals _
          |Minus|Minuss _|Number _|Obrace
          |Obraces _|Oparenthesis|Oparenthesiss _|Obracket|Obrackets _|Percent
          |Percents _|Plus|Pluss _|Question|Questions _|Quote|Quotes _|Return|Returns _
          |Semicolon|Semicolons _|Slash|Slashs _|Stars _ |Tab|Tabs _|Tilde|Tildes _|Underscore
          |Underscores _
          |Lessthan|Lessthans _|Greaterthan|Greaterthans _) as t)::tl
          ->
          main_loop (Text(htmlentities(string_of_t t))::r) [t] tl 
            
  and read_title n (r:md) (p:Md_lexer.t list) (l:Md_lexer.t list) =
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

  and rev_main_loop (r: md) (previous:Md_lexer.t list) (lexemes:Md_lexer.t list) =
    List.rev (main_loop r previous lexemes)

  (** code that starts with one or several backquote(s) *)
  and bcode (r:md) (p:Md_lexer.t list) (l:Md_lexer.t list) : md * Md_lexer.t list * Md_lexer.t list =
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

  and icode (r:md) (p:Md_lexer.t list) (l:Md_lexer.t list) : md * Md_lexer.t list * Md_lexer.t list =
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
  and new_list (r:md) (p:Md_lexer.t list) (l:Md_lexer.t list) : (md * Md_lexer.t list * Md_lexer.t list) =
    if debug then Printf.eprintf "new_list p=(%s) l=(%s)\n%!" (destring_of_tl p) (destring_of_tl l);
    begin
      let list_hd e = match e with hd::_ -> hd | _ -> assert false in
      let rec loop (ordered:bool) (result:(bool*int list*Md_lexer.t list)list) (curr_item:Md_lexer.t list) (indents:int list) (lexemes:Md_lexer.t list) =
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
                ((ordered,indents,curr_item)::(result:(bool*int list*Md_lexer.t list) list), (l: Md_lexer.t list))
            | (Newline :: e :: tl)  (* adding e to the current item *)
            | e :: tl ->
                if debug then Printf.eprintf "#%d\n%!" 8;
                loop ordered result (e::curr_item) indents tl
      in
      let rec loop2 (tmp:(bool*int list*Md_lexer.t list) list) (curr_indent:int) (ordered:bool) (accu:li list) 
          : md * (bool*int list*Md_lexer.t list) list =
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
                let item = List.rev item in
                  if debug then Printf.eprintf "@386:loop2 tmp=(%b,[],%s)::(%n)\n%!" o ((destring_of_tl item)) (List.length tl);
                  assert false
                    (* [Text("<<" ^ string_of_tl item ^ ">>")] *)
                    (* [if ordered then Ol accu else Ul accu] *)
      in
      let (tmp_r: (bool*int list*Md_lexer.t list) list), (new_l:Md_lexer.t list) = loop true [] [] [] l in
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
      let (e:md), (x:(bool*int list*Md_lexer.t list) list) = loop2 (List.rev tmp_r) (-1) false [] in
        (e@(r:md)), [], new_l
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

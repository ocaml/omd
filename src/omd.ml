(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

module Representation = Representation
module Utils = Utils
module Backend = Backend
module Parser = Parser
module Lexer = Lexer

include Representation
include Backend

let of_input lex ?extensions:e ?default_lang:d s =
  let module E = struct
    include Parser.Default_env (struct end)
    let extensions = match e with Some x -> x | None -> extensions
    let default_lang = match d with Some x -> x | None -> default_lang
  end
  in
  let module Parser = Parser.Make (E) in
  let md = Parser.parse (lex s) in
  Parser.make_paragraphs md

let of_string = of_input Lexer.lex
let of_bigarray = of_input Lexer.lex_bigarray

let to_html:
  ?override:(Representation.element -> string option) ->
  ?pindent:bool ->
  ?nl2br:bool ->
  ?cs:code_stylist -> t -> string
  =
  html_of_md

let to_text: t -> string = text_of_md

let to_markdown: t -> string = markdown_of_md

let rec set_default_lang lang = function
  | Code ("", code) :: tl ->
      Code (lang, code) :: set_default_lang lang tl
  | Code_block ("", code) :: tl ->
      Code_block (lang, code) :: set_default_lang lang tl
  (* Recurse on all elements even though code (blocks) are not allowed
     everywhere. *)
  | H (i, t) :: tl ->
      H (i, set_default_lang lang t) :: set_default_lang lang tl
  | Paragraph t :: tl ->
      Paragraph (set_default_lang lang t) :: set_default_lang lang tl
  | Emph t :: tl ->
      Emph (set_default_lang lang t) :: set_default_lang lang tl
  | Bold t :: tl ->
      Bold (set_default_lang lang t) :: set_default_lang lang tl
  | Ul t :: tl ->
      Ul (List.map (set_default_lang lang) t) :: set_default_lang lang tl
  | Ol t :: tl ->
      Ol (List.map (set_default_lang lang) t) :: set_default_lang lang tl
  | Ulp t :: tl ->
      Ulp (List.map (set_default_lang lang) t) :: set_default_lang lang tl
  | Olp t :: tl ->
      Olp (List.map (set_default_lang lang) t) :: set_default_lang lang tl
  | Url(href, t, title) :: tl ->
      Url(href, set_default_lang lang t, title) :: set_default_lang lang tl
  | Blockquote t :: tl ->
      Blockquote(set_default_lang lang t) :: set_default_lang lang tl
  (* Elements that do not contain Markdown. *)
  | (Text _ | Code _ | Code_block _ | Br | Hr | NL | Ref _ | Img_ref _ | Raw _ | Raw_block _
    |Html _ | Html_block _ | Html_comment _ | Img _ | X _) as e :: tl ->
      e :: set_default_lang lang tl
  | [] ->
      []

(* Table of contents *)

(* Given a list of headers — in the order of the document — go to the
   requested subsection.  We first seek for the [number]th header at
   [level].  *)
let rec find_start headers level number subsections =
  match headers with
  | [] -> []
  | (H (i, _), _, _) :: tl -> deal_with_header i headers tl level number subsections
  | _ :: _ -> assert false

and deal_with_header h_level headers tl level number subsections =
  if h_level > level then (* Skip, right [level]-header not yet reached. *)
    if number = 0 then
      (* Assume empty section at [level], do not consume token. *)
      (match subsections with
       | [] -> headers (* no subsection to find *)
       | n :: subsections -> find_start headers (level + 1) n subsections)
    else
      find_start tl level number subsections
  else if h_level = level then begin
    (* At proper [level].  Have we reached the [number] one? *)
    if number <= 1 then begin
      match subsections with
      | [] -> tl (* no subsection to find *)
      | n :: subsections -> find_start tl (level + 1) n subsections
    end else
      find_start tl level (number - 1) subsections
  end else (* h_level < level *)
    [] (* Sought [level] has not been found in the current section *)

(* Assume we are at the start of the headers we are interested in.
   Return the list of TOC entries for [min_level] and the [headers]
   not used for the TOC entries. *)
let rec make_toc (headers:(element*string*string)list) ~min_level ~max_level =
  if min_level > max_level then
    [], headers
  else begin
    match headers with
    | [] -> [], []
    | (H (i, t), id, _) :: tl -> toc_entry headers i t id tl ~min_level ~max_level
    | _ :: _ -> assert false
  end

and toc_entry headers h_level t id tl ~min_level ~max_level =
  if h_level > max_level then (* too deep, skip *)
    make_toc tl ~min_level ~max_level
  else if h_level < min_level then
    (* section we wanted the TOC for is finished, do not comsume the token *)
    [], headers
  else if h_level = min_level then begin
    let sub_toc, tl = make_toc tl ~min_level:(min_level + 1) ~max_level in
    let toc_entry = match sub_toc with
      | [] -> [Url("#" ^ id, t, ""); NL]
      | _ -> [Url("#" ^ id, t, ""); NL; Ul sub_toc; NL] in
    let toc, tl = make_toc tl ~min_level ~max_level in
    toc_entry :: toc, tl
  end else (* h_level > min_level *)
    let sub_toc, tl = make_toc headers ~min_level:(min_level + 1) ~max_level in
    let toc, tl = make_toc tl ~min_level ~max_level in
    [Ul sub_toc] :: toc, tl

let toc ?(start = []) ?(depth = 2) md =
  if depth < 1 then invalid_arg "Omd.toc: ~depth must be >= 1";
  let headers = Backend.headers_of_md ~remove_header_links:true md in
  let headers =
    match start with
    | [] -> headers
    | number :: subsections ->
        if number < 0 then invalid_arg("Omd.toc: level 1 start must be >= 0");
        find_start headers 1 number subsections
  in
  let len = List.length start in
  let toc, _ = make_toc headers ~min_level:(len + 1) ~max_level:(len + depth) in
  match toc with
  | [] -> []
  | _ -> [Ul(toc)]

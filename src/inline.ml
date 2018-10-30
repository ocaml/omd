(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013-2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Printf
open Utils
open Ast

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
  | Hard_break ->
      F.pp_print_string ppf "br"
  | Soft_break ->
      F.pp_print_string ppf "NL"
  | Url (_, url, _) ->
      F.fprintf ppf "@[<1>(url@ %S)@]" url
  (* | Ref (_, s) | Img_ref (_, s) -> *)
  (*     F.fprintf ppf "@[<1>(ref@ %S)@]" s *)
  | Html html ->
      F.fprintf ppf "@[<1>(html@ %S)@]" html
  (* | Raw s -> *)
  (*     F.fprintf ppf "%S" s *)
  | Img (_, src, _) ->
      F.fprintf ppf "@[<1>(img@ %S)@]" src

let rec html_of_md b md =
  let rec loop = function
    | Cat l ->
        List.iter loop l
    (* | Ref(_, _) | Img_ref (_, _) -> *)
    (*     loop (fallback#to_t) *)
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
    | Hard_break ->
        Buffer.add_string b "<br/>"
    (* | Raw s -> *)
    (*     Buffer.add_string b s *)
    | Html body ->
        Buffer.add_string b body
    | Url (s, href, title) ->
        Buffer.add_string b "<a href=\"";
        Buffer.add_string b (htmlentities ~md:true href);
        Buffer.add_string b "\"";
        begin match title with
        | None -> ()
        | Some title ->
            Buffer.add_string b " title=\"";
            Buffer.add_string b (htmlentities ~md:true title);
            Buffer.add_string b "\""
        end;
        Buffer.add_string b ">";
        html_of_md b s;
        Buffer.add_string b "</a>"
    | Soft_break ->
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
    (* | Ref (_, _, fallback) | Img_ref (_, _, fallback) -> *)
    (*     loop (Raw (fallback#to_string)) *)
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
    | Hard_break ->
        Buffer.add_string b "<br />"
    (* | Raw s -> *)
    (*     Buffer.add_string b s *)
    | Html body ->
        Buffer.add_string b body
    | Url (s, href, None) ->
        bprintf b "[%s](%s)" (markdown_of_md s) href
    | Url (s, href, Some title) ->
        bprintf b "[%s](%s \"%s\")" (markdown_of_md s) href title
    | Soft_break ->
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

let cat = function
  | [] -> Text ""
  | [x] -> x
  | l -> Cat l

type link_def = Htmllex.link_def =
  {
    label: string;
    destination: string;
    title: string option;
  }

let parse defs s =
  let lexbuf = Lexing.from_string s in
  cat (Htmllex.inline defs [] (Buffer.create 17) lexbuf)

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013-2014 by Philippe Wang <philippe.wang@cl.cam.ac.uk>         *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

open Ast

type delim =
  | Ws
  | Punct
  | Other

type t =
  | Bang_left_bracket
  | Left_bracket
  | Emph of delim * delim * Ast.emph_style * int
  | R of Ast.inline

let left_flanking = function
  | Emph (_, Other, _, _) | Emph ((Ws | Punct), Punct, _, _) -> true
  | _ -> false

let right_flanking = function
  | Emph (Other, _, _, _) | Emph (Punct, (Ws | Punct), _, _) -> true
  | _ -> false

let is_opener = function
  | Emph (pre, _, Underscore, _) as x ->
      left_flanking x && (not (right_flanking x) || pre = Punct)
  | Emph (_, _, Star, _) as x ->
      left_flanking x
  | _ ->
      false

let is_closer = function
  | Emph (_, post, Underscore, _) as x ->
      right_flanking x && (not (left_flanking x) || post = Punct)
  | Emph (_, _, Star, _) as x ->
      right_flanking x
  | _ ->
      false

let classify_delim = function
  | '!' | '"' | '#' | '$' | '%'
  | '&' | '\'' | '(' | ')' | '*' | '+'
  | ',' | '-' | '.' | '/' | ':' | ';'
  | '<' | '=' | '>' | '?' | '@' | '['
  | '\\' | ']' | '^' | '_' | '`' | '{'
  | '|' | '}' | '~' -> Punct
  | ' ' | '\t' | '\010'..'\013' -> Ws
  | _ -> Other

let to_r : _ -> Ast.inline = function
  | Bang_left_bracket -> Text "!["
  | Left_bracket -> Text "["
  | Emph (_, _, Star, n) -> Text (String.make n '*')
  | Emph (_, _, Underscore, n) -> Text (String.make n '_')
  | R x -> x

let rec parse_emph = function
  | Emph (pre, _, q1, n1) as x :: xs when is_opener x ->
      let rec loop acc = function
        | Emph (_, post, q2, n2) as x :: xs when is_closer x && q1 = q2 ->
            let xs =
              if n1 >= 2 && n2 >= 2 then
                if n2 > 2 then Emph (Punct, post, q2, n2-2) :: xs else xs
              else
                if n2 > 1 then Emph (Punct, post, q2, n2-1) :: xs else xs
            in
            let r =
              let kind = if n1 >= 2 && n2 >= 2 then Strong else Normal in
              R (Emph (kind, q1, Ast.concat (parse_emph (List.rev acc)))) :: xs
            in
            let r =
              if n1 >= 2 && n2 >= 2 then
                if n1 > 2 then Emph (pre, Punct, q1, n1-2) :: r else r
              else
                if n1 > 1 then Emph (pre, Punct, q1, n1-1) :: r else r
            in
            parse_emph r
        | x :: xs ->
            loop (x :: acc) xs
        | [] ->
            to_r x :: List.rev_map to_r acc
      in
      loop [] xs
  | x :: xs ->
      to_r x :: parse_emph xs
  | [] ->
      []

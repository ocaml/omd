open Ast

type t = inline

let concat = function
  | [x] -> x
  | l -> Concat l

let condense_ws s =
  let b = Buffer.create (String.length s) in
  let rec loop prev_ws start i =
    if i >= String.length s then
      Buffer.contents b
    else begin
      match String.get s i with
      | ' ' | '\t' | '\010'..'\013' ->
          loop true start (succ i)
      | _ as c ->
          if not start && prev_ws then Buffer.add_char b ' ';
          Buffer.add_char b c;
          loop false false (succ i)
    end
  in
  loop false true 0

let starts_with_ws s =
  if String.length s = 0 then
    false
  else begin
    match s.[0] with
    | ' ' | '\t' | '\010'..'\013' ->
        true
    | _ ->
        false
  end

let normalize x =
  let rec f x (ws, cont) =
    match x with
    | Concat l ->
        List.fold_right f l (ws, cont)
    | Text s when String.trim s = "" ->
        (ws, cont)
    | Text s ->
        let ws' = starts_with_ws s in
        let s = String.lowercase_ascii (condense_ws s) in
        let s, cont =
          match ws, cont with
          | true, Text s' :: cont ->
              String.concat " " [s; s'], cont
          | false, Text s' :: cont ->
              s ^ s', cont
          | true, cont ->
              s ^ " ", cont
          | false, cont ->
              s, cont
        in
        ws', Text s :: cont
    | Emph (k, s, x) ->
        let cont = if ws then Text " " :: cont else cont in
        let ws, x = f x (false, []) in
        ws, Emph (k, s, concat x) :: cont
    | Hard_break | Soft_break ->
        (true, cont)
    | _ ->
        assert false
  in
  let _, x = f x (false, []) in
  concat x

module Pre = struct
  type inline = t

  type delim =
    | Ws
    | Punct
    | Other

  type t =
    | Bang_left_bracket
    | Left_bracket
    | Emph of delim * delim * emph_style * int
    | R of inline

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

  let to_r : _ -> inline = function
    | Bang_left_bracket -> Text "!["
    | Left_bracket -> Text "["
    | Emph (_, _, Star, n) -> Text (String.make n '*')
    | Emph (_, _, Underscore, n) -> Text (String.make n '_')
    | R x -> x

  let rec parse_emph = function
    | Emph (pre, _, q1, n1) as x :: xs when is_opener x ->
        let rec loop acc = function
          | Emph _ as x :: xs1 as xs when is_opener x ->
              let xs' = parse_emph xs in
              if xs' = xs then loop (x :: acc) xs1 else loop acc xs'
          | Emph (_, post, q2, n2) as x :: xs when is_closer x && q1 = q2 ->
              let xs =
                if n1 >= 2 && n2 >= 2 then
                  if n2 > 2 then Emph (Punct, post, q2, n2-2) :: xs else xs
                else
                if n2 > 1 then Emph (Punct, post, q2, n2-1) :: xs else xs
              in
              let r =
                let kind = if n1 >= 2 && n2 >= 2 then Strong else Normal in
                R (Emph (kind, q1, concat (List.map to_r (parse_emph (List.rev acc))))) :: xs
              in
              let r =
                if n1 >= 2 && n2 >= 2 then
                  if n1 > 2 then Emph (pre, Punct, q1, n1-2) :: r else r
                else
                if n1 > 1 then Emph (pre, Punct, q1, n1-1) :: r else r
              in
              r
          | x :: xs ->
              loop (x :: acc) xs
          | [] ->
              x :: List.rev acc
        in
        loop [] xs
    | x :: xs ->
        x :: parse_emph xs
    | [] ->
        []
end

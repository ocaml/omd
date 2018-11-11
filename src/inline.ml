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

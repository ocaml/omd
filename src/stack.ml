(** A simple functional stack implementation *)

type 'a t = 'a list

let pop = function
  | [] -> None
  | x :: xs -> Some (x, xs)

let push = List.cons

let to_list = Fun.id

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

let map = List.map


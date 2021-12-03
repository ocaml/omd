(** A simple functional stack implementation *)

include List

type 'a t = 'a list

let pop = function
  | [] -> None
  | x :: xs -> Some (x, xs)

let push = List.cons

let to_list = List.rev

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

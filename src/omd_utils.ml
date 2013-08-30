open Printf

let debug = try ignore(Sys.getenv "DEBUG"); true with _ -> false

let raise =
  if debug then
    (fun e -> eprintf "Exception raised: %s\n%!" (Printexc.to_string e) ; raise e)
  else
    raise

module StringSet : sig
  type elt = string
  type t
  val empty : t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val union : t -> t -> t
  val of_list : elt list -> t
end = struct
  include Set.Make(struct type t = string let compare = String.compare end)
  let of_list l = List.fold_left (fun r e -> add e r) empty l
end


type 'a split = 'a list -> 'a split_action
and 'a split_action =
  | Continue
  | Continue_with of 'a list * 'a list
  | Split of 'a list * 'a list

(** [fsplit_rev ?excl ~f l] returns [Some(x,y)] where [x] is the
    **reversed** list of the consecutive elements of [l] that obey the 
    split function [f].
    Note that [f] is applied to a list of elements and not just an
    element, so that [f] can look farther in the list when applied.
    [f l] returns [Continue] if there're more elements to consume,
    [Continue_with(left,right)] if there's more elements to consume
    but we want to choose what goes to the left part and what remains
    to process (right part), and returns [Split(left,right)] if
    the splitting is decided. 
    When [f] is applied to an empty list, if it returns [Continue]
    then the result will be [None].

    If [excl] is given, then [excl] is applied before [f] is, to check
    if the splitting should be stopped right away. When the split
    fails, it returns [None]. *)
let fsplit_rev ?(excl=(fun _ -> false)) ~(f:'a split) l : ('a list * 'a list) option =
  let rec loop accu = function
    | [] ->
        begin
          match f [] with
          | Split(left, right) ->      Some(left@accu, right)
          | Continue_with(left, tl) -> loop (left@accu) tl
          | Continue ->                None
        end
    | e::tl as l ->
        if excl l then
          None
        else match f l with
          | Split(left, right) ->      Some(left@accu, right)
          | Continue_with(left, tl) -> loop (left@accu) tl
          | Continue ->                loop (e::accu) tl
  in loop [] l

(** [fsplit ?excl ~f l] returns [Some(List.rev x, y)] if [fsplit ?excl
    ~f l] returns [Some(x,y)], else it returns [None]. *)
let fsplit ?(excl=(fun _ -> false)) ~f l =
  match fsplit_rev ~excl:excl ~f:f l with
    | None -> None
    | Some(rev, l) -> Some(List.rev rev, l)

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

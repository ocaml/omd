open Printf

let debug =
  try
    ignore(Sys.getenv "DEBUG");
    eprintf "omd: debug mode activated.\n%!";
    true
  with Not_found ->
    false

let trackfix =
  try
    ignore(Sys.getenv "OMD_FIX");
    eprintf "omd: tracking mode activated: token list are very often checked, \
             it might take a *very* long time if your input is large.\n%!";
    true
  with Not_found ->
    false

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
let fsplit_rev ?(excl=(fun _ -> false)) ~(f:'a split) l
    : ('a list * 'a list) option =
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


let id_of_string ids s =
  let l = String.length s in
  let gen_id s =
    let b = Buffer.create l in
    let rec loop i flag flag2 =
      (* [flag] prevents trailing dashes;
         [flag2] prevents IDs from starting with dashes *)
      if i = l then
        ()
      else
        match s.[i] with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c ->
          (if not (flag2 || flag) then Buffer.add_char b '-');
          Buffer.add_char b c;
          loop (i+1) true true
        | _ ->
          if flag2 || flag then
            loop (i+1) false flag2
          else
            (Buffer.add_char b '-';
             loop (i+1) true flag2)
    in
    loop 0 true true;
    Buffer.contents b
  in
  let id = gen_id s in
  ids#mangle id

(* only convert when "necessary" *)
let htmlentities ?(md=false) s =
  let module Break = struct exception Break end in
  let b = Buffer.create 42 in
  let rec loop i =
    if i = String.length s then
      ()
    else
      let () =
      match s.[i] with
        | ( '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' ) as c -> Buffer.add_char b c
        | '"' -> Buffer.add_string b "&quot;"
        | '\'' -> Buffer.add_string b "&#39;"
        | '&' ->
            if md then
              begin
                try
                  let () = match s.[i+1] with
                  | '#' ->
                    let rec ff j =
                      match s.[j] with
                      | '0' .. '9' -> ff (succ j)
                      | ';' -> ()
                      | _ -> raise Break.Break
                    in
                    ff (i+2)
                  | 'A' .. 'Z' | 'a' .. 'z' ->
                    let rec ff j =
                      match s.[j] with
                      | 'A' .. 'Z' | 'a' .. 'z' -> ff (succ j)
                      | ';' -> ()
                      | _ -> raise Break.Break
                    in
                    ff (i+2)
                  | _ -> raise Break.Break
                  in                    
                  Buffer.add_string b "&"
                with _ -> Buffer.add_string b "&amp;"
              end
            else
              Buffer.add_string b "&amp;"
        | '<' -> Buffer.add_string b "&lt;"
        | '>' -> Buffer.add_string b "&gt;"
        | c -> Buffer.add_char b c
      in loop (succ i)
  in
  loop 0;
  Buffer.contents b


let minimalize_blanks s =
  let l = String.length s in
  let b = Buffer.create l in
  let rec loop f i =
    if i = l then
      Buffer.contents b
    else
      match s.[i] with
      | ' ' | '\t' | '\n' ->
        loop true (succ i)
      | c ->
        if Buffer.length b > 0 && f then
          Buffer.add_char b ' ';
        loop false (succ i)
  in loop false 0

let rec eat f = function
  | [] -> []
  | e::tl as l -> if f e then eat f tl else l

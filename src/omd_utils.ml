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
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  include Set.Make(String)
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


let rec extract_html_attributes (html:string) =
  let rec cut_on_char_from s i c =
    match String.index_from s i c with
    | 0 -> "", String.sub s 1 (String.length s - 1)
    | j -> String.sub s i (j-i), String.sub s (j+1) (String.length s - (j+1))
  in
  let remove_prefix_spaces s = 
    if s = "" then
      s 
    else if s.[0] <> ' ' then
      s
    else
      let rec loop i =
        match s.[i] with
        | ' ' -> loop (i+1)
        | _ -> String.sub s i (String.length s - i)
      in loop 1
  in
  let remove_suffix_spaces s =
    if s = "" then
      s 
    else if s.[String.length s - 1] <> ' ' then
      s
    else
      let rec loop i =
        match s.[i] with
        | ' ' -> loop (i-1)
        | _ -> String.sub s 0 (i+1)
      in loop (String.length s - 1)
  in
  let rec loop s res i =
    if i = String.length s then
      res
    else
      match
        try
          Some (take_attribute s i)
        with Not_found -> None
      with
      | Some (((_,_) as a), new_s) ->
        loop new_s (a::res) 0
      | None -> res
  and take_attribute s i =
      let name, after_eq = cut_on_char_from s i '=' in
      let name = remove_suffix_spaces name in
      let after_eq = remove_prefix_spaces after_eq in
      let value, rest = cut_on_char_from after_eq 1 after_eq.[0] in
      (name,value), remove_prefix_spaces rest
  in
  if (* Has it at least one attribute? *)
    try String.index html '>' < String.index html ' '
    with Not_found -> true
  then
    []
  else
  match html.[1] with
  | '<' | ' ' ->
    extract_html_attributes
      (remove_prefix_spaces (String.sub html 1 (String.length html - 1)))
  | _ ->
    try
      let html = snd (cut_on_char_from html 0 ' ') in
      loop (String.sub html 0 (String.index html '>')) [] 0
    with Not_found -> []

let rec extract_inner_html (html:string) =
  let rec cut_on_char_from s i c =
    match String.index_from s i c with
    | 0 -> "", String.sub s 1 (String.length s - 1)
    | j -> String.sub s i (j-i), String.sub s (j+1) (String.length s - (j+1))
  in
  let rec rcut_on_char_from s i c =
    match String.rindex_from s i c with
    | 0 -> "", String.sub s 1 (String.length s - 1)
    | j -> String.sub s 0 j, String.sub s (j+1) (String.length s - (j+1))
  in
  let _, p = cut_on_char_from html 0 '>' in
  let r, _ = rcut_on_char_from p (String.length p - 1) '<' in
  r


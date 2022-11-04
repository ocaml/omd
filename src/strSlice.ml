open Stdcompat

type t =
  { base : string
  ; off : int
  ; len : int
  }

let of_string ?(off = 0) base = { base; off; len = String.length base - off }
let to_string { base; off; len } = String.sub base off len
let print ppf s = Format.fprintf ppf "%S" (to_string s)
let get_offset { off; _ } = off
let length { len; _ } = len
let is_empty s = length s = 0

let offset n { base; off; len } =
  if n < 0 then invalid_arg "offset";
  let rec loop n base off len =
    if n = 0 || len = 0 then { base; off; len }
    else
      match base.[off] with
      | '\t' ->
          let ts = ((off + 4) / 4 * 4) - off in
          let b = Buffer.create len in
          Buffer.add_substring b base 0 off;
          for _ = 1 to ts do
            Buffer.add_char b ' '
          done;
          Buffer.add_substring b base (off + 1) (len - 1);
          loop n (Buffer.contents b) off (len + ts - 1)
      | _ -> loop (n - 1) base (off + 1) (len - 1)
  in
  loop n base off len

let lexbuf s = Lexing.from_string (to_string s)

let contains s1 { base; off; len } =
  let rec loop off =
    if off + String.length s1 > len then false
    else s1 = String.sub base off (String.length s1) || loop (off + 1)
  in
  loop off

let head = function
  | { len = 0; _ } -> None
  | { base; off; _ } -> Some base.[off]

let last = function
  | { len = 0; _ } -> None
  | { base; off; len } -> Some base.[off + len - 1]

let tail = function
  | { len = 0; _ } as s -> s
  | { base; off; len } -> { base; off = succ off; len = pred len }

let uncons s = head s |> Option.map (fun hd -> (hd, tail s))

let take n s =
  if n < 0 then invalid_arg "take";
  let rec loop n s =
    if n = 0 || length s = 0 then []
    else match head s with Some c -> c :: loop (pred n) (tail s) | None -> []
  in
  loop n s

let take_n n s =
  if n < 0 then invalid_arg "take_n";
  let len = min n s.len in
  { s with len }

let drop n s =
  if n < 0 then invalid_arg "drop";
  (* len should not be reduced below 0, as strings cannot have a negative length *)
  let len = max (s.len - n) 0 in
  (* off should not exceed the length of the base string *)
  let off = min (s.off + n) (String.length s.base) in
  { s with off; len }

let drop_last = function
  | { len = 0; _ } as s -> s
  | { base; off; len } -> { base; off; len = pred len }

let rec drop_while f s =
  match uncons s with Some (x, s') when f x -> drop_while f s' | _ -> s

let rec drop_last_while f s =
  match last s with
  | Some l when f l -> drop_last_while f (drop_last s)
  | _ -> s

let index f s =
  let len = length s in
  let rest = drop_while (fun c -> not (f c)) s in
  let idx = len - length rest in
  if idx = len then None else Some idx

(* Uncomment to test *)
(* TODO: rig up method to unit test our utilities *)
(* let () = *)
(*   let index c = index (Char.equal c) in *)
(*   let s = of_string "abcd" in *)
(*   assert (index 'a' s = Some 0); *)
(*   assert (index 'b' s = Some 1); *)
(*   assert (index 'c' s = Some 2); *)
(*   assert (index 'z' s = None) *)

let split_at f s =
  match index f s with
  | None -> (s, offset (length s) s)
  | Some idx -> ({ s with len = idx }, offset idx s)

(* Uncomment to test *)
(* TODO: rig up method to unit test our utilities *)
(* let () = *)
(*   let f x = x = 'c' in *)
(*   let before, rest = split_at f (of_string "abcdef") in *)
(*   assert ("ab" = to_string before); *)
(*   assert ("cdef" = to_string rest); *)
(*   let before, rest = split_at f (of_string "cab") in *)
(*   assert ("" = to_string before); *)
(*   assert ("cab" = to_string rest); *)
(*   let before, rest = split_at f (of_string "aaa") in *)
(*   assert ("aaa" = to_string before); *)
(*   assert ("" = to_string rest) *)

let index_unescaped sep s =
  let rec loop idx state =
    if idx = s.off+s.len then None (* If we get here and we're inside a verbatim span, what to do? *)
    else match state, s.base.[idx] with
         | `normal, '\\' -> loop (idx+1) `escape
         | `normal, '`'  -> loop (idx+1) (`verbatim_open 1)
         | `normal, c when c = sep -> Some (idx-s.off)
         | `normal, _ -> loop (idx+1) `normal
         | `escape, _ -> loop (idx+1) `normal
         | `verbatim_open n, '`'   -> loop (idx+1) (`verbatim_open (n+1))
         | `verbatim_open n, _     -> loop (idx+1) (`within_verbatim n)
         | `within_verbatim 1, '`' -> loop (idx+1) `normal
         | `within_verbatim n, '`' -> loop (idx+1) (`verbatim_close (n,n-1))
         | `within_verbatim n, _   -> loop (idx+1) (`within_verbatim n)
         | `verbatim_close (_, 1), '`' -> loop (idx+1) `normal
         | `verbatim_close (n, k), '`' -> loop (idx+1) (`verbatim_close (n,k-1))
         | `verbatim_close (n, _), _   -> loop (idx+1) (`within_verbatim n)
  in
  loop s.off `normal

let exists f s =
  let rec loop s i =
    if i >= s.len then false
    else if f s.base.[s.off + i] then true
    else loop s (succ i)
  in
  loop s 0

let for_all f s = not (exists (fun c -> not (f c)) s)

let sub ~len s =
  if len > s.len then invalid_arg "sub";
  { s with len }

let fold_left f init s =
  let rec aux acc rest =
    match uncons rest with None -> acc | Some (x, xs) -> aux (f x acc) xs
  in
  aux init s

(* let () = *)
(*   let s = of_string "abcde" in *)
(*   assert (fold_left (fun _ n -> n + 1) 0 s = 5); *)
(*   assert (fold_left (fun c s -> String.make 2 c ^ s) "" s = "eeddccbbaa") *)

let trim s =
  let is_whitespace = function
    | ' ' | '\t' | '\010' .. '\013' -> true
    | _ -> false
  in
  drop_while is_whitespace (drop_last_while is_whitespace s)

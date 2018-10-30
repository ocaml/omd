type t = int * string

let of_string s = (0, s)
let to_string (off, s) = String.sub s off (String.length s - off)

let offset n (off, s) =
  if n < 0 then invalid_arg "offset";
  let rec loop n off s =
    if n = 0 || off >= String.length s then
      (off, s)
    else begin
      match s.[off] with
      | '\t' ->
          let ts = (off + 4) / 4 * 4 - off in
          let b = Buffer.create (String.length s) in
          Buffer.add_substring b s 0 off;
          for _ = 1 to ts do Buffer.add_char b ' ' done;
          Buffer.add_substring b s (off + 1) (String.length s - off - 1);
          loop n off (Buffer.contents b)
      | _ ->
          loop (n - 1) (off + 1) s
          (* | _ -> *)
          (*     String.sub s i (String.length s - i) *)
    end
  in
  loop n off s

let lexbuf (off, s) =
  let off = ref off in
  Lexing.from_function (fun b n ->
      let n = min n (String.length s - !off) in
      Bytes.blit_string s !off b 0 n;
      off := n + !off;
      n
    )

let contains s1 (off, s) =
  let rec loop off =
    if off + String.length s1 > String.length s then
      false
    else
      s1 = String.sub s off (String.length s1) || loop (off + 1)
  in
  loop off

let head (off, s) =
  if off >= String.length s then
    None
  else
    Some (s.[off], (succ off, s))

let tail ((off, s) as x) =
  if off < String.length s then
    (succ off, s)
  else
    x

let take n (off, s) =
  if n < 0 || off + n > String.length s then invalid_arg "take";
  (0, String.sub s off n)

let span f (off, s) =
  let rec loop n =
    if off + n >= String.length s || not (f s.[off + n]) then
      (0, String.sub s off n), (off + n, s)
    else
      loop (succ n)
  in
  loop 0

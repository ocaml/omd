type t =
  {
    base: string;
    off: int;
    len: int;
  }

let of_string base =
  {base; off = 0; len = String.length base}

let to_string {base; off; len} =
  String.sub base off len

let print ppf s =
  Format.fprintf ppf "%S" (to_string s)

let length {len; _} =
  len

let offset n {base; off; len} =
  if n < 0 then invalid_arg "offset";
  let rec loop n base off len =
    if n = 0 || len = 0 then
      {base; off; len}
    else begin
      match base.[off] with
      | '\t' ->
          let ts = (off + 4) / 4 * 4 - off in
          let b = Buffer.create len in
          Buffer.add_substring b base 0 off;
          for _ = 1 to ts do Buffer.add_char b ' ' done;
          Buffer.add_substring b base (off + 1) (len - 1);
          loop n (Buffer.contents b) off (len + ts - 1)
      | _ ->
          loop (n - 1) base (off + 1) (len - 1)
    end
  in
  loop n base off len

let lexbuf s =
  Lexing.from_string (to_string s)

let contains s1 {base; off; len} =
  let rec loop off =
    if off + String.length s1 > len then
      false
    else
      s1 = String.sub base off (String.length s1) || loop (off + 1)
  in
  loop off

let head ?rev s =
  match rev, s with
  | _, {len = 0; _} ->
      None
  | None, {base; off; _} ->
      Some base.[off]
  | Some (), {base; off; len} ->
      Some base.[off + len - 1]

let tail ?rev s =
  match rev, s with
  | _, {len = 0; _} ->
      s
  | None, {base; off; len} ->
      {base; off = succ off; len = pred len}
  | Some (), {base; off; len} ->
      {base; off; len = pred len}

let heads n s =
  let rec loop n s =
    if n = 0 || length s = 0 then [], s
    else let x = head s in let l, s = loop (pred n) (tail s) in x :: l, s
  in
  loop n

let is_empty s =
  length s = 0

let exists f s =
  let rec loop s i =
    if i >= s.len then
      false
    else if f s.base.[s.off + i] then
      true
    else
      loop s (succ i)
  in
  loop s 0

let for_all f s =
  not (exists (fun c -> not (f c)) s)

let sub ~len s =
  if len > s.len then invalid_arg "sub";
  {s with len}

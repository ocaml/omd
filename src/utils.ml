let id_of_string ids s =
  let n = String.length s in
  let out = Buffer.create 0 in
  (* Put [s] into [b], replacing non-alphanumeric characters with dashes. *)
  let rec loop started i =
    if i = n then ()
    else begin
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as c ->
          Buffer.add_char out c ;
          loop true (i + 1)
      (* Don't want to start with dashes. *)
      | _ when not started ->
          loop false (i + 1)
      | _ ->
          Buffer.add_char out '-' ;
          loop false (i + 1)
    end
  in
  loop false 0 ;
  let s' = Buffer.contents out in
  if s' = "" then ""
  else
    (* Find out the index of the last character in [s'] that isn't a dash. *)
    let last_trailing =
      let rec loop i =
        if i < 0 || s'.[i] <> '-' then i
        else loop (i - 1)
      in
      loop (String.length s' - 1)
    in
    (* Trim trailing dashes. *)
    ids#mangle @@ String.sub s' 0 (last_trailing + 1)

(* only convert when "necessary" *)
let htmlentities s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then
      Buffer.contents b
    else begin
      begin match s.[i] with
      | '"' ->
          Buffer.add_string b "&quot;"
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | c ->
          Buffer.add_char b c
      end;
      loop (succ i)
    end
  in
  loop 0

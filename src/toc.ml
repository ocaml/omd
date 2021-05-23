open Ast

let headers doc =
  let headers = ref [] in
  let rec loop blocks =
    List.iter (function
        | Heading (_, level, inline) -> headers := (level, inline, "TODO id") :: !headers
        | Blockquote (_, blocks) -> loop blocks
        | List (_, _, _, block_lists) -> List.iter loop block_lists
        | Paragraph _
        | Thematic_break _
        | Html_block _
        | Definition_list _
        | Code_block _ -> ()
      ) blocks
  in
  loop doc;
  List.rev !headers

(* Given a list of headers — in the order of the document — go to the
   requested subsection.  We first seek for the [number]th header at
   [level].  *)
let rec find_start headers level number subsections =
  match headers with
  | (header_level, _, _) :: tl when header_level > level ->
      (* Skip, right [level]-header not yet reached. *)
      if number = 0 then
        (* Assume empty section at [level], do not consume token. *)
        (match subsections with
         | [] -> headers (* no subsection to find *)
         | n :: subsections -> find_start headers (level + 1) n subsections)
      else find_start tl level number subsections
  | (header_level, _, _) :: tl when header_level = level ->
      (* At proper [level].  Have we reached the [number] one? *)
      if number <= 1 then (
        match subsections with
        | [] -> tl (* no subsection to find *)
        | n :: subsections -> find_start tl (level + 1) n subsections
      )
      else find_start tl level (number - 1) subsections
  | _ ->
      (* Sought [level] has not been found in the current section *)
      []

(* Assume we are at the start of the headers we are interested in.
   Return the list of TOC entries for [min_level] and the [headers]
   not used for the TOC entries. *)
let rec make_toc (headers: (int * 'a inline * string) list) ~min_level ~max_level =
  match headers with
  | _ when min_level > max_level -> [], headers
  | [] -> [], []
  | (level, _, _) :: _ when level < min_level -> [], headers
  | (level, _, _) :: tl when level > max_level ->  make_toc tl ~min_level ~max_level
  | (level, t, id) :: tl when level = min_level ->
      let sub_toc, tl = make_toc tl ~min_level:(min_level + 1) ~max_level in
      let toc_entry =
        match sub_toc with
        | [] -> [ Paragraph ([], Link([], {label=t; destination="#" ^ id; title=None})) ]
        | _ -> [
            Paragraph ([], Link([], {label=t; destination="#" ^ id; title=None}));
            List ([], Bullet '*', Tight, sub_toc);
          ]
      in
      let toc, tl = make_toc tl ~min_level ~max_level in
      toc_entry :: toc, tl
  | _ ->
      let sub_toc, tl = make_toc headers ~min_level:(min_level + 1) ~max_level in
      let toc, tl = make_toc tl ~min_level ~max_level in
      [List ([], Bullet '*', Tight, sub_toc)] :: toc, tl

let toc ?(start=[]) ?(depth=2) doc =
  if depth < 1 then invalid_arg "Omd.toc: ~depth must be >= 1";
  let headers = headers (* ~remove_header_links:true *) doc in
  let headers =
    match start with
    | [] -> headers
    | number :: _ when number < 0 -> invalid_arg("Omd.toc: level 1 start must be >= 0");
    | number :: subsections -> find_start headers 1 number subsections
  in
  let len = List.length start in
  let toc, _ = make_toc headers  ~min_level:(len + 1) ~max_level:(len + depth) in
  match toc with
  | [] -> []
  | _ -> [List([], Bullet '*', Tight, toc)]

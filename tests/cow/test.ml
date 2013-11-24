open Printf

(* Compare the structures with the exception of the objects references
   may contain. *)
let rec equal m1 m2 =
  try List.for_all2 equal_el m1 m2
  with Invalid_argument _ -> false (* different lengths *)

and equal_el md1 md2 =
  let open Omd in
  match md1, md2 with
  | H1 m1, H1 m2 | H2 m1, H2 m2 | H3 m1, H3 m2 | H4 m1, H4 m2 | H5 m1, H5 m2
  | H6 m1, H6 m2 | Paragraph m1, Paragraph m2 | Emph m1, Emph m2
  | Bold m1, Bold m2 ->
     equal m1 m2
  | Text s1, Text s2 -> s1 = s2
  | Ul m1, Ul m2 | Ol m1, Ol m2 | Ulp m1, Ulp m2 | Olp m1, Olp m2 ->
     (try List.for_all2 equal m1 m2
      with Invalid_argument _ -> false)
  | Code(n1, c1), Code(n2, c2) | Code_block(n1, c1), Code_block(n2, c2) ->
     n1 = n2 && c1 = c2
  | Br, Br | Hr, Hr | NL, NL -> true
  | Url(h1, m1, t1), Url(h2, m2, t2) ->
     h1 = h2 && equal m1 m2 && t1 = t2
  | Ref(_, n1, s1, _), Ref(_, n2, s2, _) -> n1 = n2 && s1 = s2
  | Img_ref(_, n1, a1, _), Img_ref(_, n2, a2, _) -> n1 = n2 && a1 = a2
  | Html s1, Html s2 | Html_block s1, Html_block s2
  | Html_comment s1, Html_comment s2 ->
     s1 = s2
  | Blockquote m1, Blockquote m2 -> equal m1 m2
  | Img(a1, s1, t1), Img(a2, s2, t2) -> a1 = a2 && s1 = s2 && t1 = t2
  | X x1, X x2 -> x1#name = x2#name
  | _, _ -> false


let dir =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    Filename.concat (Sys.getcwd ()) "tests/cow"

let slurp filename =
  let file = open_in filename in
  let size = in_channel_length file in
  let buf = String.create size in
  begin
    really_input file buf 0 size;
    buf
  end

let remove_blank s =
  let b = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | ' ' | '\t' | '\n' -> ()
        | c -> Buffer.add_char b c
    done;
    Buffer.contents b

let process successes failures file =
  let html = (Filename.chop_extension file) ^ ".html" in
  if not (Sys.file_exists html) then (
    Printf.eprintf "File %s does not exist.\n" html;
    exit 2;
  );
  let expected = slurp html in
  let md, observed =
   try
     let md = Omd.of_string (slurp file) in
     md, Omd.(to_html md)
   with e -> [], Printexc.to_string e
  in
  (* Make sure a round trip produce identical results *)
  let round_trip = Omd.of_string(Omd.to_markdown md) in
  if expected <> observed && remove_blank expected <> remove_blank observed then (
    eprintf "FAILURE: %s\n" file;
    eprintf "  expected = %S\n" (expected);
    eprintf "  observed = %S\n" (observed);
    incr failures
  )
  else if not(equal md round_trip) then (
    eprintf "FAILURE: %s\n" file;
    eprintf "  Omd.of_string(Omd.to_markdown md) <> md\n";
    incr failures
  )
  else (
    eprintf "SUCCESS: %s\n" file;
    incr successes
  )

let () =
  prerr_endline ("Reading directory " ^ dir);
  let files = Array.to_list (Sys.readdir dir) in
  let files = List.map (fun f -> Filename.concat dir f) files in
  let md_files = List.filter (fun f -> Filename.check_suffix f ".md") files in
  let successes = ref 0
  and failures = ref 0 in
  List.iter (process successes failures) md_files;
  eprintf "%i test passed;  %i tests failed.\n" !successes !failures

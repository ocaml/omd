open Printf

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
  let observed =
   try
    Omd.(to_html (Omd.of_string (slurp file)))
   with e -> Printexc.to_string e
  in
  if expected = observed || remove_blank expected = remove_blank observed then (
    eprintf "SUCCESS: %s\n" file;
    incr successes
  )
  else (
    eprintf "FAILURE: %s\n" file;
    eprintf "expected = %S\n" (expected);
    eprintf "observed = %S\n" (observed);
    incr failures
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

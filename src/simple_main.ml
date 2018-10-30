let main () =
  let input = ref [] in
  let output = ref "" in
  let spec =
    let open Arg in
    [
      "-o", Set_string output,
      " file.html Specify the output file (default is stdout).";

      "--", Rest(fun s -> input := s :: !input),
      " Consider all remaining arguments as input file names.";

      "-version", Unit(fun () -> print_endline "This is version VERSION."; exit 0),
      " Print version.";
    ]
  in
  Arg.parse (Arg.align spec) (fun s -> input := s :: !input) "omd [options] [inputfile1 .. inputfileN] [options]";
  let output = if !output = "" then stdout else open_out_bin !output in
  let process ic =
    let md = Omd.of_channel ic in
    let html = Omd.to_html md in
    output_string output html;
    flush output
    (* if false && Utils.debug then *)
    (*   print_endline (Backend.sexpr_of_md (Omd.Parser.default_parse (preprocess(Lexer.lex (Buffer.contents b))))) *)
  in
  if !input = [] then
    process stdin
  else
    List.iter (fun filename ->
        let ic = open_in filename in
        match process ic with
        | () -> close_in ic
        | exception e -> close_in_noerr ic; raise e
      ) !input

let () =
  try
    main ()
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      exit 1

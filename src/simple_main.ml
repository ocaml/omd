open Omd

module E = Parser.Default_env(struct end)

let sexp = ref false

let main () =
  let input = ref [] in
  let output = ref "" in
  let spec =
    let open Arg in
    [
      "-sexp", Set sexp,
      " Emit sexp";
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
    let module T = Text.Make (Text.Default_env (struct end)) in
    let md = Block.of_channel ic in
    let md = List.map (Block.map ~f:(fun s -> T.parse (Lexer.lex s))) md in
    if !sexp then
      Format.eprintf "@[<v>%a@]@."
        (Format.pp_print_list ~pp_sep:Format.pp_print_space (Block.print Text.print)) md
    else begin
      let html = Block.to_html Text.html_of_md md in
      output_string output html;
      flush output
    end
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
  | Utils.Error msg when not Utils.debug ->
      Printf.eprintf "(OMD) Error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

exception Break

let old () =
  let b = Buffer.create 42 in
  try while true do
      Buffer.add_char b (input_char stdin)
    done; assert false
  with End_of_file ->
    let open Omd in
    print_endline (to_html
                     (make_paragraphs
                        (parse (lex (Buffer.contents b)))));
    try
      (try ignore (Sys.getenv "DEBUG") with
        Not_found -> raise Break
      );
      print_endline
        (Omd_backend.sexpr_of_md
           (Omd_parser.parse (Omd_lexer.lex (Buffer.contents b))))
    with Break -> ()
;;

let main () =
  let input = ref []
  and output = ref ""
  in
  Arg.(
    parse
      (align[
        "-o", Set_string output, "file.html Specify the output file (default is stdout).";
        "--", Rest(fun s -> input := s :: !input), " Consider all remaining arguments as input file names.";
        "-u", Clear(Omd_parser.gh_uemph_or_bold_style), " Use standard Markdown style for emph/bold when using `_'.";
        "-b", Set(Omd_parser.blind_html), " Don't check validity of HTML tag names.";
        "-x", String(fun s -> ignore s), "ext Activate extension ext (not yet implemented).";
        "-l", ignore, " List available extensions ext (not yet implemented).";
        "-s", Set(Omd_parser.strict_html), " (might not work as expected yet) Block HTML only in block HTML, inline HTML only in inline HTML (semantics undefined if use both -b and -s).";
      ])
      (fun s -> input := s :: !input)
      "omd [inputfile1 .. inputfileN] [-o outputfile]
omd [-o outputfile] [inputfile1 .. inputfileN]
omd [-o outputfile] [-- inputfile1 .. inputfileN]"
  );
  let input_files =
    if !input = [] then
      [stdin]
    else
      List.rev_map (open_in) !input
  in
  let output =
    if !output = "" then
      stdout
    else
      open_out_bin !output
  in
  List.iter (fun ic ->
    let b = Buffer.create 42 in
    try while true do
        Buffer.add_char b (input_char ic)
      done; assert false
    with End_of_file ->
      let open Omd in
      output_string output
        (to_html
           (make_paragraphs
              (parse (* ~extension:(Omd_xtxt.get()) *) (lex (Buffer.contents b)))));
      flush output;
      try
        (try ignore (Sys.getenv "DEBUG") with
          Not_found -> raise Break
        );
        print_endline
          (Omd_backend.sexpr_of_md
             (Omd_parser.parse (Omd_lexer.lex (Buffer.contents b))))
      with Break -> ()
  )
    input_files

let () =
  try
    main ()
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

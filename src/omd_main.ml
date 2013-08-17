(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

let old () =
  let b = Buffer.create 42 in
  try while true do
      Buffer.add_char b (input_char stdin)
    done; assert false
  with End_of_file ->
    let open Omd in
    print_endline (Omd.to_html
                     (make_paragraphs
                        (parse (lex (Buffer.contents b)))));
    try
      (try ignore (Sys.getenv "DEBUG") with
        Not_found -> failwith "2HUIDNBIU2Y782HUIDBZUDBEUBEUB"
      );
      print_endline
        (Omd_backend.sexpr_of_md
           ((* Md_backend.make_paragraphs *)
             (Omd_parser.parse (Omd_lexer.lex (Buffer.contents b)))))
    with Failure "2HUIDNBIU2Y782HUIDBZUDBEUBEUB" -> ()
;;

let main () =
  let input = ref []
  and output = ref ""
  in
  Arg.(
    parse
      (align[
        "-o", Set_string output, "file.html Specify the output file (default is stdout).";
        "-u", Clear(Omd_parser.gh_uemph_or_bold_style), " Use standard Markdown style for emph/bold when using `_'.";
        "--", Rest(fun s -> input := s :: !input), " Consider all remaining arguments as input file names.";
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
        (Omd.to_html
           (make_paragraphs
              (parse (lex (Buffer.contents b)))));
      flush output;
      try
        (try ignore (Sys.getenv "DEBUG") with
          Not_found -> failwith "2HUIDNBIU2Y782HUIDBZUDBEUBEUB"
        );
        print_endline
          (Omd_backend.sexpr_of_md
             ((* Md_backend.make_paragraphs *)
               (Omd_parser.parse (Omd_lexer.lex (Buffer.contents b)))))
      with Failure "2HUIDNBIU2Y782HUIDBZUDBEUBEUB" -> ()
  )
    input_files

let () =
  try
    main ()
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)
open Omd

exception Break

let remove_comments l =
  let open Omd in
  let rec loop = function
    | true, Exclamations n :: tl when n > 0 ->
        loop (true,
              Omd_parser.eat (function Newline|Newlines _ -> false|_-> true) tl)
    | _, (Newline|Newlines _ as e)::tl ->
        e::loop (true, tl)
    | _, e::tl ->
        e::loop (false, tl)
    | _, [] -> []
  in loop (true, l)

let remove_endline_comments l =
  let open Omd in
  let rec loop = function
    | Exclamations n :: tl when n > 0 ->
        loop (Omd_parser.eat (function Newline|Newlines _ -> false|_-> true) tl)
    | (Newline|Newlines _ as e)::tl ->
        e::loop tl
    | e::tl ->
        e::loop tl
    | [] -> []
  in loop l

let preprocess_functions = ref []
let (++) a b = a := b :: !a

let preprocess l =
  List.fold_left (fun r e -> e r)
    l
    !preprocess_functions
    

let main () =
  let input = ref []
  and output = ref ""
  in
  Arg.(
    parse
      (align[
        "-o", Set_string output,
         "file.html Specify the output file (default is stdout).";
        "--", Rest(fun s -> input := s :: !input),
         " Consider all remaining arguments as input file names.";
        "-u", Clear(Omd_parser.gh_uemph_or_bold_style),
         " Use standard Markdown style for emph/bold when using `_'.";
        "-c", Unit(fun () -> preprocess_functions ++ remove_endline_comments),
         " Ignore lines that start with `!!!' (3 or more exclamation points).";
        "-C", Unit(fun () -> preprocess_functions ++ remove_comments),
         " Ignore everything on a line after `!!!' (3 or more exclamation points).";
        "-b", Set(Omd_parser.blind_html),
         " Don't check validity of HTML tag names.";
        "-x", String(fun s -> ignore s),
         "ext Activate extension ext (not yet implemented).";
        "-l", Unit ignore,
         " List available extensions ext (not yet implemented).";
        "-s", Set(Omd_parser.strict_html),
         " (might not work as expected yet) Block HTML only in block HTML, \
           inline HTML only in inline HTML \
           (semantics undefined if use both -b and -s).";
      ])
      (fun s -> input := s :: !input)
      "omd [options] [inputfile1 .. inputfileN] [options]"
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
              (parse (* ~extension:(Omd_xtxt.get()) *)
                   (preprocess(lex (Buffer.contents b))))));
      flush output;
      try
        (try ignore (Sys.getenv "DEBUG") with
          Not_found -> raise Break
        );
        print_endline
          (Omd_backend.sexpr_of_md
             (Omd_parser.parse (preprocess(Omd_lexer.lex (Buffer.contents b)))))
      with Break -> ()
  )
    input_files

let () =
  try
    main ()
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

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
    | Backslash :: (Exclamations n as e) :: tl when n > 0 ->
      e :: loop tl
    | Backslashs b :: (Exclamations n as e) :: tl when n > 0 && b mod 2 = 1 ->
      Backslashs(b-1) :: e :: loop tl
    | Exclamations n :: tl when n > 0 ->
      loop (Omd_parser.eat (function Newline|Newlines _ -> false|_-> true) tl)
    | e::tl ->
      e::loop tl
    | [] -> []
  in loop l

(* [minimalize_blanks s] returns [s] where the first and last
   characters are never blank, and two consecutive blanks never
   happen. *)
let minimalize_blanks s =
  let l = String.length s in
  let b = Buffer.create l in
  let rec loop f i =
    if i = l then
      Buffer.contents b
    else
      match s.[i] with
      | ' ' | '\t' | '\n' ->
        loop true (succ i)
      | c ->
        if Buffer.length b > 0 && f then
          Buffer.add_char b ' ';
        loop false (succ i)
  in loop false 0

let preprocess_functions = ref []
let (++) a b = a := b :: !a

let preprocess l =
  List.fold_left (fun r e -> e r)
    l
    !preprocess_functions


let otoc = ref false
let toc = ref false
let omarkdown = ref false
let notags = ref false

let make_toc md =
  (* bad performance but particularly simple to implement *)
  let b = Buffer.create 42 in
  let rec loop = function
    | (H1 e, id, ih) :: tl ->
      Printf.bprintf b "* [%s](#%s)\n" ih id;
      loop tl
    | (H2 e, id, ih) :: tl ->
      Printf.bprintf b " * [%s](#%s)\n" ih id;
      loop tl
    | (H3 e, id, ih) :: tl ->
      Printf.bprintf b "  * [%s](#%s)\n" ih id;
      loop tl
    | (H4 e, id, ih) :: tl ->
      Printf.bprintf b "   * [%s](#%s)\n" ih id;
      loop tl
    | (H5 e, id, ih) :: tl ->
      Printf.bprintf b "    * [%s](#%s)\n" ih id;
      loop tl
    | (H6 e, id, ih) :: tl ->
      Printf.bprintf b "     * [%s](#%s)\n" ih id;
      loop tl
    | [] -> ()
    | _ -> assert false
  in
  loop (Omd_backend.headers_of_md md);
  parse(lex(Buffer.contents b))

let tag_toc l =
  if !toc then
    let rec loop = function
      | Star::
          Word "Table"::Space::
          Word "of"::Space::
          Word "contents"::Star::tl ->
        Tag(fun r p l ->
          Some(X(
                object
                    (* to prevent endless loops *)
                  val mutable shield = false
                  method name="toc"
                  method to_html ?indent f md =
                    if shield || not !toc then
                      None
                    else
                      begin
                        shield <- true;
                        let r = f (make_toc md) in
                        shield <- false;
                        Some r
                      end
                  method to_sexpr f md =
                    if shield || not !toc then
                      None
                    else
                      begin
                        shield <- true;
                        let r = f (make_toc md) in
                        shield <- false;
                        Some r
                      end
                  method to_t md =
                    if shield || not !toc then
                      None
                    else
                      begin
                        shield <- true;
                        let r = (make_toc md) in
                        shield <- false;
                        Some r
                      end
                end)::r,p,l)) :: loop tl
      | e::tl -> e::loop tl
      | [] -> []
    in loop l
  else
    l

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
        "-m", Set(omarkdown), " Output Markdown instead of HTML (not yet implemented).";
        "-notags", Set(notags), " Output without the HTML tags.";
        "-toc", Set(toc), "n Replace `*Table of contents*' by the table of contents of depth n.";
        "-otoc", Set(otoc), "f Only output the table of contents to file f instead of inplace.";

        "-x", String(ignore),
        "ext Activate extension ext (not yet implemented).";
        "-l", Unit ignore,
        " List available extensions ext (not yet implemented).";

        "-b", Set(Omd_parser.blind_html),
        " Don't check validity of HTML tag names.";
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
        ((if !notags then to_text else to_html)
            ((if !otoc then make_toc else make_paragraphs)
                (parse (* ~extension:(Omd_xtxt.get()) *)
                   (preprocess(lex (Buffer.contents b)))))
        );
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

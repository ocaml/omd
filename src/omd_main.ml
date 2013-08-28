(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(** This module implements a end-user interface for OMD.  It mainly
    uses the module [Omd], which implements the main interface to the
    library OMD. There are still a few uses of other modules from the
    OMD library but this may change.

    Treatments that are not specific to Markdown (such as table of
    contents generation) are done here. If you want to build an
    alternative end-user Markdown tool using OMD, you might want to
    fork this file or get inspiration from it.

    Happy coding!
*)


open Omd

(** [remove_comments l] returns [l] without OMD comments. *)
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

(** [remove_endline_comments l] returns [l] without OMD endline-comments. *)
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

(** [minimalize_blanks s] returns [s] where the first and last
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

(** [preprocess_functions] contains the list of preprocessing functions *)
let preprocess_functions = ref []

(** [a ++ b] is a shortcut for [a := b :: !a] *)
let (++) a b = a := b :: !a

(** [preprocess l] returns [l] to which all preprocessing functions
    (in reference [preprocess_functions]) have been applied. *)
let preprocess l =
  List.fold_left (fun r e -> e r)
    l
    !preprocess_functions

(** flag: output the table of contents only. *)
let otoc = ref false

(** flag: replace "*Table of contents*" by the table of contents. *)
let toc = ref false

(** flag: output Markdown instead of HTML. *)
let omarkdown = ref false

(** flag: output HTML but without HTML tags, so it's not really HTML anymore. *)
let notags = ref false

(** flag: depth of table of contents *) 
let toc_depth = ref 2

(** flag: first header level for table of contents *) 
let toc_start = ref 1

(** flag: for multiple dashes in HTML comments, replace dashes by &#45;  *)
let protect_html_comments = ref false

(** [make_toc ?(start_level=1) ?(depth=2) md] returns a table of
    contents when [md] is a list of section headers. If [md] contains
    tags other than section header tags, an exception is raised.
 *)
let make_toc ?(start_level=1) ?(depth=2) md =
  (* probably poor performance but particularly simple to implement *)
  let b = Buffer.create 42 in
  let rec loop = function
    | (H1 e, id, ih) :: tl ->
      if start_level <= 1 && start_level + depth > 1 then
      Printf.bprintf b "* [%s](#%s)\n" ih id;
      loop tl
    | (H2 e, id, ih) :: tl ->
      if start_level <= 2 && start_level + depth > 2 then
      Printf.bprintf b " * [%s](#%s)\n" ih id;
      loop tl
    | (H3 e, id, ih) :: tl ->
      if start_level <= 3 && start_level + depth > 3 then
      Printf.bprintf b "  * [%s](#%s)\n" ih id;
      loop tl
    | (H4 e, id, ih) :: tl ->
      if start_level <= 4 && start_level + depth > 4 then
      Printf.bprintf b "   * [%s](#%s)\n" ih id;
      loop tl
    | (H5 e, id, ih) :: tl ->
      if start_level <= 5 && start_level + depth > 5 then
      Printf.bprintf b "    * [%s](#%s)\n" ih id;
      loop tl
    | (H6 e, id, ih) :: tl ->
      if start_level <= 6 && start_level + depth > 6 then
      Printf.bprintf b "     * [%s](#%s)\n" ih id;
      loop tl
    | [] -> ()
    | _ -> failwith "Omd_main.make_toc: wrong argument, \
                     please read the documentation and/or file a bug report."
  in
  loop(Omd_backend.headers_of_md md);
  parse(lex(Buffer.contents b))

(** [patch_html_comments l] returns the list [l] where
    all [Html_comments s] have been converted to [Html_comments s'],
    where [s'] means [s] with dashes replaced by &#45; except for
    single dashes (which are left untouched).

    N.B. It seems that it's not valid to have double dashes inside HTML comments
    (cf. http://validator.w3.org/check). So one way to make life somewhat easier
    is to patch the comments and transform inner dashed to &#45;.  *)
let patch_html_comments l =
  let htmlcomments s =
    let b = Buffer.create (String.length s) in
      for i = 0 to 3 do
        Buffer.add_char b s.[i]
      done;
      for i = 4 to String.length s - 4 do
        match s.[i] with
          | '-' as c ->
              if (i > 4 && s.[i-1] = '-')
                || (i < String.length s - 5 && s.[i+1] = '-')
              then
                Printf.bprintf b "&#%d;" (int_of_char c)
              else
                Buffer.add_char b c
          | c -> Buffer.add_char b c
      done;
      for i = String.length s - 3 to String.length s - 1 do
        Buffer.add_char b s.[i]
      done;
      Buffer.contents b
  in
  let rec loop accu = function
  | Html_comments s :: tl ->
      loop (Html_comments(htmlcomments s)::accu) tl
  | e :: tl ->
      loop (e :: accu) tl
  | [] -> List.rev accu
  in loop [] l


(** [tag_toc l] returns [l] where *Table of contents* has been replaced
    by a tag that can generate a table of contents. *)
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
                  (* [shield] is used to prevent endless loops.
                     If one wants to use system threads at some point, 
                     and calls methods of this object  concurrently,
                     then there is a real problem. *)
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

(** main function *)
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
        "-ts", Set_int(toc_start), "f Table of contents minimum level.";
        "-td", Set_int(toc_depth), "f Table of contents depth.";
        "-H", Set(protect_html_comments), " Protect HTML comments.";
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
      let lexed = lex (Buffer.contents b) in
      let preprocessed = preprocess lexed in
      let parsed1 = parse preprocessed in
      let parsed2 =
        if !protect_html_comments then 
          patch_html_comments parsed1
        else
          parsed1
      in
      let parsed = parsed2 in
      let o1 = (* make either TOC or paragraphs *)
        (if !otoc then make_toc ~start_level:!toc_start ~depth:!toc_depth else make_paragraphs)
          parsed in
      let o2 = (* output either Text or HTML *)
        (if !notags then to_text else to_html) o1
      in
        output_string output o2;
        flush output;
        if Omd_utils.debug then
          print_endline
            (Omd_backend.sexpr_of_md
               (parse (preprocess(lex (Buffer.contents b)))));
  )
    input_files


(* call the main function *)
let () =
  try
    main ()
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

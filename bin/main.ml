type output_mode =
  | Html
  | Markdown

let with_open_in fn f =
  let ic = open_in fn in
  match f ic with
  | r ->
      close_in ic;
      r
  | exception e ->
      close_in_noerr ic;
      raise e

let with_open_out fn f =
  let oc = open_out fn in
  match f oc with
  | r ->
      close_out oc;
      r
  | exception e ->
      close_out_noerr oc;
      raise e

let process mode ic oc =
  let md = Omd.of_channel ic in
  let s =
    match mode with
    | Html -> Omd.to_html md
    | Markdown -> Omd.to_markdown md
  in
  output_string oc s

let input = ref []

let output = ref ""

let output_mode = ref Html

let spec =
  [ ( "-o"
    , Arg.Set_string output
    , " file.html Specify the output file (default is stdout)." )
  ; ( "--mode"
    , String
        (function
        | "html" -> output_mode := Html
        | "markdown" -> output_mode := Markdown
        | _ -> raise (Arg.Bad "Accepted modes are html and markdown"))
    , " html|markdown Specify the output mode (default is html)." )
  ; ( "--"
    , Rest (fun s -> input := s :: !input)
    , " Consider all remaining arguments as input file names." )
  ]

let main () =
  Arg.parse
    (Arg.align spec)
    (fun s -> input := s :: !input)
    "omd [options] [inputfile1 .. inputfileN] [options]";
  let with_output f =
    if !output = "" then
      f stdout
    else
      with_open_out !output f
  in
  with_output @@ fun oc ->
  if !input = [] then
    process !output_mode stdin oc
  else
    let f filename =
      with_open_in filename @@ fun ic -> process !output_mode ic oc
    in
    List.(iter f (rev !input))

let () =
  try main () with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1

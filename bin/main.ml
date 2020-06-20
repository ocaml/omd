let with_open_in fn f =
  let ic = open_in fn in
  match f ic with
  | r -> close_in ic; r
  | exception e -> close_in_noerr ic; raise e

let with_open_out fn f =
  let oc = open_out fn in
  match f oc with
  | r -> close_out oc; r
  | exception e -> close_out_noerr oc; raise e

let process ic oc =
  let md = Omd.of_channel ic in
  output_string oc (Omd.to_html md)

let input = ref []

let output = ref ""

let spec =
  [
    "-o", Arg.Set_string output,
    " file.html Specify the output file (default is stdout).";

    "--", Rest(fun s -> input := s :: !input),
    " Consider all remaining arguments as input file names.";

    "-version", Unit(fun () -> print_endline "This is version VERSION."; exit 0),
    " Print version.";
  ]

let main () =
  Arg.parse (Arg.align spec)
    (fun s -> input := s :: !input)
    "omd [options] [inputfile1 .. inputfileN] [options]";
  let with_output f =
    if !output = "" then
      f stdout
    else
      with_open_out !output f
  in
  with_output @@ fun oc ->
  if !input = [] then process stdin oc
  else begin
    let f filename = with_open_in filename @@ fun ic -> process ic oc in
    List.iter f !input
  end

let () =
  try
    main ()
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1

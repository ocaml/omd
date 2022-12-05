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

let process ?auto_identifiers ic oc =
  let md = Omd.of_channel ic in
  output_string oc (Omd.to_html ?auto_identifiers md)

let print_version () =
  let version =
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v
  in
  print_endline version;
  exit 0

let input = ref []
let output = ref ""
let auto_identifiers = ref None

let spec =
  [ ( "-o"
    , Arg.Set_string output
    , " file.html Specify the output file (default is stdout)." )
  ; ( "--auto-identifiers"
    , Arg.Bool (fun x -> auto_identifiers := Some x)
    , " Should identifiers be automatically assigned to headings." )
  ; ( "--version"
    , Arg.Unit print_version
    , " Display the version of the currently installed omd." )
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
    if !output = "" then f stdout else with_open_out !output f
  in
  let auto_identifiers = !auto_identifiers in
  with_output @@ fun oc ->
  if !input = [] then process ?auto_identifiers stdin oc
  else
    let f filename =
      with_open_in filename @@ fun ic -> process ?auto_identifiers ic oc
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

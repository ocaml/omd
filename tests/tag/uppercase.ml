type format =
  | Sexp
  | Html
  | Markdown
  | Text

let fmt = ref Html

let convert ast =
  match !fmt with
  | Html -> Omd.to_html ast
  | Sexp -> Omd.to_sexp ast
  | Markdown | Text -> assert false

let main () =
  let input = ref [] in
  let output = ref "" in
  let spec =
    let open Arg in
    [
      "-o", Set_string output,
      " file.html Specify the output file (default is stdout).";

      "-sexp", Unit (fun () -> fmt := Sexp), " sexp";

      "--", Rest(fun s -> input := s :: !input),
      " Consider all remaining arguments as input file names.";

      "-version", Unit(fun () -> print_endline "This is version VERSION."; exit 0),
      " Print version.";
    ]
  in
  Arg.parse (Arg.align spec) (fun s -> input := s :: !input) "omd [options] [inputfile1 .. inputfileN] [options]";
  let output = if !output = "" then stdout else open_out_bin !output in
  let printer =
    let print_upper_text p b t =
      let t = String.uppercase_ascii t in
      Omd.default_printer.text p b t
    in
    let print_upper_code p b (c: Omd.Code.t) =
      let c = {c with content = String.uppercase_ascii c.content} in
      Omd.default_printer.code p b c
    in
    let print_tag (p: Omd.printer) b (t: 'inline Omd.Tag.t) =
      match t.tag with
      | "capitalize" ->
        p.inline {p with text = print_upper_text; code = print_upper_code} b t.content
      | _ -> Omd.default_printer.tag p b t
    in
    let print_tag_block (p: Omd.printer) b t =
      match t.Omd.Tag_block.tag with
      | "capitalize" ->
        let f i block =
          p.block {p with text = print_upper_text; code = print_upper_code} b block;
          if i < List.length t.content - 1 then
            Buffer.add_char b '\n'
        in
        List.iteri f t.content
      | _ -> Omd.default_printer.tag_block p b t
    in
    {Omd.default_printer with tag = print_tag; tag_block = print_tag_block}
  in
  let process ic =
    let md = Omd.of_channel ic in
    output_string output (Omd.to_html ~printer md);
    flush output
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
      Printexc.print_backtrace stderr;
      exit 1

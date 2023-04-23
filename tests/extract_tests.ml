(* Extract test cases from Spec *)
let protect ~finally f =
  match f () with
  | exception e ->
      finally ();
      raise e
  | r ->
      finally ();
      r

let disabled = []

(* Some pp tests won't work because of escaping characters *)
let pp_disabled =
  [ 006
  ; 012
  ; 014
  ; 015
  ; 017
  ; 020
  ; 039
  ; 040
  ; 041
  ; 081
  ; 082
  ; 095
  ; 128
  ; 174
  ; 175
  ; 194
  ; 195
  ; 202
  ; 228
  ; 229
  ; 230
  ; 232
  ; 236
  ; 238
  ; 244
  ; 252
  ; 255
  ; 259
  ; 260
  ; 264
  ; 276
  ; 312
  ; 319
  ; 320
  ; 321
  ; 324
  ; 325
  ; 329
  ; 330
  ; 331
  ; 339
  ; 346
  ; 349
  ; 416
  ; 435
  ; 444
  ; 446
  ; 447
  ; 456
  ; 458
  ; 488
  ; 492
  ; 505
  ; 508
  ; 514
  ; 525
  ; 528
  ; 531
  ; 532
  ; 537
  ; 549
  ; 592
  ; 602
  ; 605
  ]

let pp_disabled_filename = [ "gfm_table_spec"; "extra_table_test"; "def_list" ]

let with_open_in fn f =
  let ic = open_in fn in
  protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)

let with_open_out fn f =
  let oc = open_out fn in
  protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let begins_with s s' =
  String.length s >= String.length s' && String.sub s 0 (String.length s') = s'

let test_delim = "````````````````````````````````"
let tab_re = Str.regexp_string "→"
let insert_tabs s = Str.global_replace tab_re "\t" s

type test =
  { filename : string
  ; example : int
  ; markdown : string
  ; html : string
  }

let add_line buf l =
  Buffer.add_string buf (insert_tabs l);
  Buffer.add_char buf '\n'

let parse_test_spec filename =
  let buf = Buffer.create 256 in
  with_open_in filename @@ fun ic ->
  let rec go tests example =
    match input_line ic with
    | exception End_of_file -> List.rev tests
    | line ->
        if begins_with line test_delim then begin
          Buffer.clear buf;
          let rec get_test () =
            let line = input_line ic in
            if line = "." then begin
              let markdown = Buffer.contents buf in
              Buffer.clear buf;
              let rec get_html () =
                let line = input_line ic in
                if begins_with line test_delim then
                  let html = Buffer.contents buf in
                  { filename; example; markdown; html }
                else begin
                  add_line buf line;
                  get_html ()
                end
              in
              get_html ()
            end
            else begin
              add_line buf line;
              get_test ()
            end
          in
          go (get_test () :: tests) (succ example)
        end
        else go tests example
  in
  go [] 1

let write_dune_file test_specs tests =
  let pp ppf { filename; example; _ } =
    let base = Filename.remove_extension filename in
    Format.fprintf ppf "@ %s-%03d.md %s-%03d.html" base example base example
  in
  Format.printf
    "@[<v1>(rule@ @[<hov1>(deps %s)@]@ @[<v1>(targets%t)@]@ @[<hov1>(action@ \
     (run ./extract_tests.exe -generate-test-files %%{deps}))@])@]@."
    (String.concat " " test_specs)
    (fun ppf -> List.iter (pp ppf) tests);
  List.iter
    (fun { filename; example; _ } ->
      let base = Filename.remove_extension filename in
      Format.printf
        "@[<v1>(rule@ @[<hov1>(action@ @[<hov1>(with-stdout-to \
         %s-%03d.html.new@ @[<hov1>(run@ ./omd.exe@ \
         %%{dep:%s-%03d.md})@])@])@])@]@."
        base
        example
        base
        example;
      if
        not
          (List.mem example pp_disabled
          || pp_disabled_filename
             |> List.exists (fun pp_disabled_filename ->
                    String.starts_with ~prefix:pp_disabled_filename filename))
      then
        Format.printf
          "@[<v1>(rule@ @[<hov1>(action@ @[<hov1>(progn \
           @[<hov1>(with-stdout-to %s-%03d.md.pp@ @[<hov1>(run@ ./omd_pp.exe \
           print %%{dep:%s-%03d.md}))@]@ (with-stdout-to %s-%03d.html.pp.new@ \
           @[<hov1>(run@ ./omd_pp.exe html@ %s-%03d.md.pp)@])@])@])@])@]@."
          base
          example
          base
          example
          base
          example
          base
          example;
      Format.printf
        "@[<v1>(rule@ @[<hov1>(alias %s-%03d)@]@ @[<hov1>(action@ \
         @[<hov1>(diff@ %s-%03d.html %s-%03d.html.new)@])@])@]@."
        base
        example
        base
        example
        base
        example;
      if
        not
          (List.mem example pp_disabled
          || pp_disabled_filename
             |> List.exists (fun pp_disabled_filename ->
                    String.starts_with ~prefix:pp_disabled_filename filename))
      then
        Format.printf
          "@[<v1>(rule@ @[<hov1>(alias %s-%03d)@]@ @[<hov1>(action@ \
           @[<hov1>(diff@ %s-%03d.html %s-%03d.html.pp.new)@])@])@]@."
          base
          example
          base
          example
          base
          example)
    tests;
  let pp ppf { filename; example; _ } =
    let base = Filename.remove_extension filename in
    if not (List.mem example disabled) then
      Format.fprintf ppf "@ (alias %s-%03d)" base example
  in
  Format.printf
    "@[<v1>(alias@ (name runtest)@ @[<v1>(deps%t)@])@]@."
    (fun ppf -> List.iter (pp ppf) tests)

let li_begin_re = Str.regexp_string "<li>\n"
let li_end_re = Str.regexp_string "\n</li>"

let normalize_html s =
  Str.global_replace li_end_re "</li>" (Str.global_replace li_begin_re "<li>" s)

let generate_test_files tests =
  let f { filename; example; markdown; html } =
    let base = Filename.remove_extension filename in
    with_open_out (Printf.sprintf "%s-%03d.md" base example) (fun oc ->
        output_string oc markdown);
    with_open_out (Printf.sprintf "%s-%03d.html" base example) (fun oc ->
        output_string oc (normalize_html html))
  in
  List.iter f tests

type mode =
  | Generate_test_files
  | Write_dune_file

let mode = ref None

let spec =
  let set x () = mode := Some x in
  [ ( "-generate-test-files"
    , Arg.Unit (set Generate_test_files)
    , " Generate test files" )
  ; ("-write-dune-file", Arg.Unit (set Write_dune_file), " Write dune file")
  ]

let test_specs = ref []
let add_to_list l x = l := x :: !l

let () =
  Arg.parse (Arg.align spec) (add_to_list test_specs) "";
  let test_specs = List.rev !test_specs in
  let tests = List.flatten (List.map parse_test_spec test_specs) in
  match !mode with
  | None -> ()
  | Some Generate_test_files -> generate_test_files tests
  | Some Write_dune_file -> write_dune_file test_specs tests

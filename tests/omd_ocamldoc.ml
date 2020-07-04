let with_open_in fn f =
  let ic = open_in fn in
  let result = f ic in
  close_in_noerr ic;
  result

let () =
  with_open_in Sys.argv.(1) @@ fun ic ->
  print_string (Omd.to_ocamldoc (Omd.of_channel ic))

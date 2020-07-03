let li_begin_re = Str.regexp_string "<li>\n"
let li_end_re = Str.regexp_string "\n</li>"

let normalize_html s =
  Str.global_replace li_end_re "</li>"
    (Str.global_replace li_begin_re "<li>" s)

let with_open_in fn f =
  let ic = open_in fn in
  let result = f ic in
  close_in_noerr ic;
  result

let () =
  with_open_in Sys.argv.(1) @@ fun ic ->
  print_string (normalize_html (Omd.to_html (Omd.of_channel ic)))

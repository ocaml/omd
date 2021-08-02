let protect ~finally f =
  match f () with
  | exception e ->
      finally ();
      raise e
  | r ->
      finally ();
      r

let li_begin_re = Str.regexp_string "<li>\n"

let li_end_re = Str.regexp_string "\n</li>"

let normalize_html s =
  Str.global_replace li_end_re "</li>" (Str.global_replace li_begin_re "<li>" s)

let with_open_in fn f =
  let ic = open_in fn in
  protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)

(* Originally I had been using:
   print_string (normalize_html (Omd.to_html (Omd.of_string (to_string (Omd.of_channel ic))))) but it seems 
   the of_channel and of_string can sometimes provide different results! spec-142 of_string added an extra newline
   afaict, so to try and be as consistent as possible in this test, I'm writing it back to another file and 
   then re-reading it using of_channel... *)

let out_string () = 
  with_open_in Sys.argv.(2) @@ fun ic1 ->
  let to_string omd = 
    Omd.Print.pp Format.str_formatter omd;
    Format.flush_str_formatter ()
  in
  let s = to_string (Omd.of_channel ic1) in 
   print_string s

let html_check () = 
  with_open_in (Sys.argv.(2)) @@ fun ic1 ->
  let html = normalize_html (Omd.(to_html (of_channel ic1))) in 
    print_string html

let () =
  match Sys.argv.(1) with 
    | "print" -> out_string ()
    | "html" -> html_check ()
    | _ -> failwith "usage: print parses the markdown and prints the string, html prints the html"
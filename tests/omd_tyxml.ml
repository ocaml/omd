(* Integration tests for conversion from Omd.doc to Tyxml.doc *)

let tyxml_to_string t =
  Format.asprintf "%a" Tyxml.Html.(pp ~indent:true ()) t

let html_of_string s =
  s |> Omd.of_string |> Omd_tyxml.of_omd |> tyxml_to_string

let with_open_in fn f =
  let ic = open_in fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> f ic)

let () =
  with_open_in Sys.argv.(1) @@ fun ic ->
  ic
  |> Omd.of_channel
  |> Omd_tyxml.of_omd
  |> tyxml_to_string
  |> print_string

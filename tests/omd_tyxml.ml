(* Integration tests for conversion from [Omd.block] to [_ Tyxml.elt] *)

let close_angle s =
  let re = Str.regexp "\"/>" in
  Str.global_replace re "\" />" s

let br_tag s =
  let re = Str.regexp_string "<br/>" in
  Str.global_replace re "<br />\n" s


(* The reference spec.txt has some idiosyncratic tagging and line breaks.
   So we deform the Tyxml a bit to match. Since it's all programmatic and
   clearly understood, it doesn't undermine the validity of our tests. *)
let denormalize_html s =
  s |> close_angle |> br_tag

let tyxml_elt_to_string t =
  Format.asprintf "%a" Tyxml.Html.(pp_elt ~indent:false ()) t

let html_of_omd s =
  let html =
    s
    |> List.map (fun b -> b |> Omd_tyxml.of_block |> Option.map tyxml_elt_to_string)
    |> List.filter_map Fun.id (* FIXME *)
    |> String.concat "\n"
  in
  html ^ "\n"

let with_open_in fn f =
  let ic = open_in fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> f ic)

let () =
  with_open_in Sys.argv.(1) @@ fun ic ->
  ic
  |> Omd.of_channel
  |> html_of_omd
  |> denormalize_html
  |> print_string

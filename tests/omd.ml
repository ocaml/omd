let with_open_in fn f =
  let ic = open_in fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> f ic)

(* FIXME: Resolve preferred backend *)

let replacements =
  [ Str.regexp_string "<br>", "<br>\n"
  ; Str.regexp_string "</pre><pre ", "</pre>\n<pre "
  ]


let tyxml_elt_to_string t =
  Format.asprintf "%a" Tyxml.Html.(pp_elt ~indent:false ()) t

let html_of_omd s =
  s
  |> List.map (fun b -> b |> Omd_tyxml.of_block |> tyxml_elt_to_string)
  |> String.concat ""

let denormalize_html str =
  List.fold_left (fun s (re, rep) -> Str.global_replace re rep s) str replacements

let () =
  with_open_in Sys.argv.(1) @@ fun ic ->
  ic
  |> Omd.of_channel
  |> html_of_omd
  |> Common.normalize_html
  |> denormalize_html
  |> print_string

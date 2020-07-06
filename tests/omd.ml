(* let li_begin_re = Str.regexp_string "<li>\n"
 * let li_end_re = Str.regexp_string "\n</li>" *)

(* let normalize_html s =
 *   Str.global_replace li_end_re "</li>"
 *     (Str.global_replace li_begin_re "<li>" s) *)

let with_open_in fn f =
  let ic = open_in fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> f ic)

(* FIXME: Resolve preferred backend *)

(* FIXME: This is getting rediculous. Probably better, imo, to programmatically
   format the spec HTML and compare the ASTs of the HTMl instead of doing this
   string munging *)

let replacements =
  [ Str.regexp_string "<br>", "<br>\n"
  (* ; Str.regexp_string ">\n\n</a>", ">\n</a>" *)
  (* ; Str.regexp "\n\n$", "\n" *)
  (*   Str.regexp "\"/>", "\" />"
   * ; Str.regexp_string "<br/>", "<br />\n"
   * ; Str.regexp_string "<ul>", "<ul>\n"
   * ; Str.regexp_string "</li>", "</li>\n"
   * ; Str.regexp_string "</p><ul>", "</p>\n<ul>"
   * ; Str.regexp_string "</ul><p>", "</ul>\n<p>"
   * ; Str.regexp_string "<ol><", "<ol>\n<"
   * ; Str.regexp_string "<ul><", "<ul>\n<" *)
  ]


let tyxml_elt_to_string t =
  Format.asprintf "%a" Tyxml.Html.(pp_elt ~indent:false ()) t

let html_of_omd s =
  s
  |> List.map (fun b -> b |> Omd_tyxml.of_block |> tyxml_elt_to_string)
  |> String.concat ""

let normalize_html s = Soup.(parse s |> pretty_print)

let denormalize_html str =
  List.fold_left (fun s (re, rep) -> Str.global_replace re rep s) str replacements

let () =
  with_open_in Sys.argv.(1) @@ fun ic ->
  ic
  |> Omd.of_channel
  |> html_of_omd
  |> normalize_html
  |> denormalize_html
  |> print_string

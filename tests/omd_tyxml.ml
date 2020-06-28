(* Unit tests for conversion from Omd.t to Tyxml.t *)

let tyxml_to_string t =
  Format.asprintf "%a" Tyxml.Html.(pp ()) t

let html_of_string s =
  s |> Omd.of_string |> Omd_tyxml.of_omd |> tyxml_to_string

let test name ~actual ~expected =
  try
    assert (actual = expected)
  with Assert_failure (file, line, _) ->
    Printf.eprintf "Test '%s' failed (file %s, line %d):\n  expected: %s\n  actual: %s\n\n"
      name file line expected actual

let () =
  test "the empty document"
    ~actual:(html_of_string "")
    ~expected:{|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml"><head><title></title></head><body></body></html>|};

  test "a plain line"
    ~actual:(html_of_string "a plain line")
    ~expected:{|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml"><head><title></title></head><body><p>a plain line</p></body></html>|};

  test "emphasized text"
    ~actual:(html_of_string "some *emphasized* text")
    ~expected:{||}

(* TODO(shon) Tie this into the generated specs tests *)
(* let li_begin_re = Str.regexp_string "<li>\n"
 * let li_end_re = Str.regexp_string "\n</li>"
 *
 * let normalize_html s =
 *   Str.global_replace li_end_re "</li>"
 *     (Str.global_replace li_begin_re "<li>" s)
 *
 * let with_open_in fn f =
 *   let ic = open_in fn in
 *   Fun.protect ~finally:(fun () -> close_in_noerr ic)
 *     (fun () -> f ic)
 *
 * let () =
 *   with_open_in Sys.argv.(1) @@ fun ic ->
 *   ic
 *   |> Omd.of_channel
 *   |> Omd_tyxml.of_omd
 *   |> tyxml_to_string
 *   |> normalize_html
 *   |> print_string *)

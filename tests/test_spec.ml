(* This file tests the conformity of the generated AST with Markdown.  *)

open Printf

let success = ref 0
let failures = ref 0

let () =
  let report () =
    if !failures = 0 then
      printf "Congratulation, all tests passed!"
    else
      printf "%d test%s passed, %d test%s failed.\n"
             !success (if !success > 1 then "s" else "")
             !failures (if !failures > 1 then "s" else "") in
  at_exit report

let test name md_string desired_md =
  try
    let md = Omd.of_string md_string in
    if md = desired_md then (
      incr success;
      printf "%s: SUCCESS\n" name
    )
    else (
      incr failures;
      printf "%s: FAILURE\n" name
    )
  with e ->
    incr failures;
    printf "%s: EXCEPTION\n  %s\n" name (Printexc.to_string e)


       let () =
  let open Omd in
  (* Paragraphs and Line Breaks
   ***********************************************************************)
  (* "A paragraph is simply one or more consecutive lines of text,
     separated by one or more blank lines."  Note that the final
     newlines are not considered to be part of the paragraphs, just a
     delimiter. *)
  test "Paragraph, simple" "Paragraph1\nline2\n\nP2\n\n\nP3"
       [Paragraph [Text "Paragraph1"; NL; Text "line2"];
        Paragraph [Text "P2"]; Paragraph [Text "P3"]];
  (* A blank line is any line that looks like a blank line — a line
     containing nothing but spaces or tabs is considered blank. *)
  test "Paragraph, blank line" "P1\n   \nP2\n\t\nP3\n"
       [Omd.Paragraph [Omd.Text "P1"];
        Omd.Paragraph [Omd.Text "P2"];
        Omd.Paragraph [Omd.Text "P3"]];
  (* "When you do want to insert a <br />, you end a line with two or
     more spaces." *)
  test "Paragraph, <br>" "Paragraph1  \nline2\n\nParagraph2"
       [Paragraph [Text "Paragraph1"; Br; Text "line2"];
        Paragraph [Text "Paragraph2"]];

  (* Normal paragraphs should not be indented with spaces or tabs. *)

  (* Headers
   ***********************************************************************)
  test "header, ===" "Title\n=="  [Omd.H1 [Omd.Text "Title"]];
  test "header, ===" "Title\n---" [Omd.H2 [Omd.Text "Title"]];

  test "header, #" "# Title" [Omd.H1 [Omd.Text "Title"]];
  test "header, ##" "## Title" [Omd.H2 [Omd.Text "Title"]];
  test "header, ###" "### Title" [Omd.H3 [Omd.Text "Title"]];
  test "header, ####" "#### Title" [Omd.H4 [Omd.Text "Title"]];
  test "header, #####" "##### Title" [Omd.H5 [Omd.Text "Title"]];
  test "header, ######" "###### Title" [Omd.H6 [Omd.Text "Title"]];
  test "header, too deep" "######## Title\n" [Omd.H6 [Omd.Text "Title"]];
  test "header, + space" "# Title  " [Omd.H1 [Omd.Text "Title"]];
  test "header, # #" "# Title ###" [Omd.H1 [Omd.Text "Title"]];
  test "header, # #" "# Title # " [Omd.H1 [Omd.Text "Title"]];

  (* Blockquotes
   ***********************************************************************)

  test "blockquote, simple" "> quoted"
       [Blockquote [Paragraph [Text "quoted"]]];
  test "blockquote, simple 2" "> quoted\n"
       [Blockquote [Paragraph [Text "quoted"]]];
  test "blockquote, 2 pars" "> quoted\n>\n> quoted2"
       [Blockquote [Paragraph [Text "quoted"];
                    Paragraph [Text "quoted2"]]];
  test "blockquote, 2 pars (blank line)" "> quoted\n\n> quoted2"
       [Blockquote [Paragraph [Text "quoted"];
                    Paragraph [Text "quoted2"]]];

  test "blockquote + header" "> ## header\n"
       [Blockquote [H2 [Text "header"]]];
  test "blockquote + header + par" "> ## header\nHello"
       [Blockquote [H2 [Text "header"]];  Paragraph [Text "Hello"]];
  test "blockquote + header + par" "> ## header\n> Hello"
       [Blockquote [H2 [Text "header"];  Paragraph [Text "Hello"]]];
  test "blockquote + list" "> 1. item1\n> 2. item2\n"
       [Blockquote [Ol [[Text "item1"; NL]; (* FIXME: shoule NL go? *)
                        [Text "item2"; NL]]]];
  test "blockquote + code (4 spaces)" ">     code"
       [Blockquote [Code_block ("", "code")]];
  test "blockquote + code (tab)" "> \tcode"
       [Blockquote [Code_block ("", "code")]];
  test "blockquote + code ```" "> ```\n> code\n> ```"
       [Blockquote [Code_block ("", "code\n")]];


  (* Code
   ***********************************************************************)

  test "code dashes" "```\n--\n--\n--\n```"
       [Omd.Code_block ("", "\n--\n--\n--\n")]

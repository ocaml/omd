(* This file tests the conformity of the generated AST with Markdown.  *)

open Printf
open Omd

let success = ref 0
let failures = ref 0

let () =
  let report () =
    if !failures = 0 then
      printf "Congratulation, all %d specification tests passed!\n" !success
    else
      printf "%d test%s passed, %d test%s failed.\n"
             !success (if !success > 1 then "s" else "")
             !failures (if !failures > 1 then "s" else "") in
  at_exit report

let test name md_string desired_md =
  try
    let md = of_string md_string in
    if md = desired_md then (
      incr success;
      (* printf "%s: SUCCESS\n" name *)
    )
    else (
      incr failures;
      printf "%s: FAILURE\n" name;
      printf "   input = %S\nexpected = %S\n  result = %S\n"
        md_string
        (to_sexp desired_md)
        (to_sexp md)
    )
  with e ->
    incr failures;
    printf "%s: EXCEPTION\n  %s\n" name (Printexc.to_string e)


let () =
  (* Paragraphs and Line Breaks
   ***********************************************************************)
  (* "A paragraph is simply one or more consecutive lines of text,
     separated by one or more blank lines."  Note that the final
     newlines are not considered to be part of the paragraphs, just a
     delimiter. *)
  test "Paragraph, simple" "Paragraph1\nline2\n\nP2\n\n\nP3"
       [Paragraph (Concat [Text "Paragraph1"; Soft_break; Text "line2"]);
        Paragraph (Text "P2"); Paragraph (Text "P3")];
  (* A blank line is any line that looks like a blank line — a line
     containing nothing but spaces or tabs is considered blank. *)
  test "Paragraph, blank line" "P1\n   \nP2\n\t\nP3\n"
       [Paragraph (Text "P1");
        Paragraph (Text "P2");
        Paragraph (Text "P3")];
  (* "When you do want to insert a <br />, you end a line with two or
     more spaces." *)
  test "Paragraph, <br>" "Paragraph1  \nline2\n\nParagraph2"
       [Paragraph (Concat [Text "Paragraph1"; Hard_break; Text "line2"]);
        Paragraph (Text "Paragraph2")];

  (* Normal paragraphs should not be indented with spaces or tabs. *)

  (* Headers
   ***********************************************************************)
  test "header, ===" "Title\n=="  [Heading (1, Text "Title")];
  test "header, ---" "Title\n---" [Heading (2, Text "Title")];

  test "header, #" "# Title" [Heading (1, Text "Title")];
  test "header, ##" "## Title" [Heading (2, Text "Title")];
  test "header, ###" "### Title" [Heading (3, Text "Title")];
  test "header, ####" "#### Title" [Heading (4, Text "Title")];
  test "header, #####" "##### Title" [Heading (5, Text "Title")];
  test "header, ######" "###### Title" [Heading (6, Text "Title")];
  test "header, too deep" "######## Title\n"
    [Paragraph (Text "######## Title")];
  test "header, # + space" "# Title  " [Heading (1, Text "Title")];
  test "header, # #" "# Title ###" [Heading (1, Text "Title")];
  test "header, # #" "# Title # " [Heading (1, Text "Title")];
  test "header, ## + space" "## Title #  " [Heading (2, Text "Title")];

  test "header, # + \\n" "# Title\n" [Heading (1, Text "Title")];
  test "header, # + space + \\n" "# Title  \n" [Heading (1, Text "Title")];
  test "header, # + # + \\n" "# Title # \n" [Heading (1, Text "Title")];


  (* Blockquotes
   ***********************************************************************)

  test "blockquote, simple" "> quoted"
       [Blockquote [Paragraph (Text "quoted")]];
  test "blockquote, simple 2" "> quoted\n"
       [Blockquote [Paragraph (Text "quoted")]];
  test "blockquote, 2 pars" "> quoted\n>\n> quoted2"
       [Blockquote [Paragraph (Text "quoted");
                    Paragraph (Text "quoted2")]];
  test "blockquote, 2 pars (blank line)" "> quoted\n> \n> quoted2"
       [Blockquote [Paragraph (Text "quoted");
                    Paragraph (Text "quoted2")]];

  test "blockquote + header" "> ## header\n"
       [Blockquote [Heading (2, (Text "header"))]];
  test "blockquote + header + par" "> ## header\nHello"
       [Blockquote [Heading (2, (Text "header")); Paragraph (Text "Hello")]];
  test "blockquote + header + par" "> ## header\n> Hello"
       [Blockquote [Heading (2, Text "header"); Paragraph (Text "Hello")]];
  test "blockquote + list" "> 1. item1\n> 2. item2\n"
       [Blockquote [List (Ordered (1, '.'), Tight, [[Paragraph (Text "item1")];
                        [Paragraph (Text "item2")]])]];
  test "blockquote + code (4 spaces)" ">     code"
       [Blockquote [Code_block (None, Some "code")]];
  test "blockquote + code (tab)" "> \tcode"
       [Blockquote [Code_block (None, Some "code")]];
  test "blockquote + code ```" "> ```\n> code\n> ```"
       [Blockquote [Code_block (Some (Backtick, ""), Some "code")]];


  (* Lists
   ***********************************************************************)

  test "list, simple" "8.  Red\n1.  Green\n3.  Blue"
       [List (Ordered (8, '.'), Tight, [[Paragraph (Text "Red")]; [Paragraph (Text "Green")]; [Paragraph (Text "Blue")]])];
  test "list, simple2" "\n8.  Red\n1.  Green\n3.  Blue"
       [List (Ordered (8, '.'), Tight, [[Paragraph (Text "Red")]; [Paragraph (Text "Green")]; [Paragraph (Text "Blue")]])];
  test "list, par" "8.  Red\n\n1.  Green\n\n3.  Blue"
       [List (Ordered (8, '.'), Loose, [[Paragraph (Text "Red")]; [Paragraph (Text "Green")];
             [Paragraph (Text "Blue")]])];

  test "list, *" "* AA\n* VV"
       [List (Unordered '*', Tight, [[Paragraph (Text "AA")]; [Paragraph (Text "VV")]])];
  test "list, 2 levels" "* AA\n\n* VV"
       [List (Unordered '*', Loose, [[Paragraph (Text "AA")]; [Paragraph (Text "VV")]])];

  test "list + code + space + header" "- A
- B

    ```
    code
    ```

# header"
       [List (Unordered '-', Loose, [[Paragraph (Text "A")];
             [Paragraph (Text "B"); Code_block (Some (Backtick, ""), Some "code")]]);
        Heading (1, Text "header")];

  (* Code
   ***********************************************************************)

  test "code dashes" "```\n--\n--\n--\n```"
       [Code_block (Some (Backtick, ""), Some "--\n--\n--")]

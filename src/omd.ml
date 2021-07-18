module Pre = Block.Pre
include Ast

type doc = attributes block list

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

let parse_inlines (md, defs) =
  let defs =
    let f (def : attributes link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  List.map (Mapper.map (parse_inline defs)) md

let of_channel ic = parse_inlines (Pre.of_channel ic)

let of_string s = parse_inlines (Pre.of_string s)

let to_html doc = Html.to_string (Html.of_doc doc)

let to_sexp ast = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

let headers = Toc.headers

let toc = Toc.toc

let%expect_test "Example usage: toc" =
  let md =
    of_string
      {|
# Header 1
Section 1 text {#sec1}

# Header 2 {#sec2}
Section 2 text

## Header 2.1
Section  2.1 text

## Header 2.2
Section  2.2 text
### Header 2.2.1 {#sec2.2.1}
### Header 2.2.2

# Header 3 with [link](www.wikipedia.com) {#sec3}
  |}
  in
  let print_html doc = print_endline (to_html doc) in

  (* By default, toc scans the full document, but only the first 2 levels of headers *)
  print_html (toc md);
  [%expect
    {|
    <ul>
    <li>Header 1
    </li>
    <li><a href="#sec2">Header 2</a>
    <ul>
    <li>Header 2.1
    </li>
    <li>Header 2.2
    </li>
    </ul>
    </li>
    <li><a href="#sec3">Header 3 with link</a>
    </li>
    </ul> |}];

  (* With depth set to 1 we get only the top level headers *)
  print_html (toc ~depth:1 md);
  [%expect
    {|
    <ul>
    <li>Header 1
    </li>
    <li><a href="#sec2">Header 2</a>
    </li>
    <li><a href="#sec3">Header 3 with link</a>
    </li>
    </ul> |}];

  (* With depth set to a higher value, we get all headers in the example *)
  print_html (toc ~depth:3 md);
  [%expect
    {|
    <ul>
    <li>Header 1
    </li>
    <li><a href="#sec2">Header 2</a>
    <ul>
    <li>Header 2.1
    </li>
    <li>Header 2.2
    <ul>
    <li><a href="#sec2.2.1">Header 2.2.1</a>
    </li>
    <li>Header 2.2.2
    </li>
    </ul>
    </li>
    </ul>
    </li>
    <li><a href="#sec3">Header 3 with link</a>
    </li>
    </ul> |}];

  (* The start parameter allows to narrow the toc to a specific subtree *)
  print_html (toc ~start:[ 2 ] ~depth:3 md);
  [%expect
    {|
    <ul>
    <li>Header 2.1
    </li>
    <li>Header 2.2
    <ul>
    <li><a href="#sec2.2.1">Header 2.2.1</a>
    </li>
    <li>Header 2.2.2
    </li>
    </ul>
    </li>
    </ul> |}];

  (* The start parameter can be arbitrarily deep *)
  print_html (toc ~start:[ 2; 2 ] ~depth:3 md);
  [%expect
    {|
    <ul>
    <li><a href="#sec2.2.1">Header 2.2.1</a>
    </li>
    <li>Header 2.2.2
    </li>
    </ul> |}]

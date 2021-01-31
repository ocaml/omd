include Ast

let read_all ic =
  let buf = Buffer.create 1024 in
  let stop = ref false in
  while not !stop do
    try Buffer.add_channel buf ic 1024 with End_of_file -> stop := true
  done;
  Buffer.contents buf

let of_string s =
  Option.get (Parser.T.run Parser.doc s)

let of_channel ic =
  of_string (read_all ic)

let to_html doc =
  Html.to_string (Html.of_doc doc)

(* let to_sexp ast = *)
(*   Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast) *)

(* The document model *)

include Ast.Impl

(* Helper functions for construction document AST *)

module Ctor = Ast_constructors.Impl

(* Table of contents *)

let headers = Toc.headers
let toc = Toc.toc

(* Conversion *)

let parse_inline defs s = Parser.inline defs (Parser.P.of_string s)

let trim_start_while p s =
  let start = ref true in
  let b = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> Buffer.add_string b s
      | `Uchar u when p u && !start -> ()
      | `Uchar u when !start ->
          start := false;
          Uutf.Buffer.add_utf_8 b u
      | `Uchar u -> Uutf.Buffer.add_utf_8 b u)
    ()
    s;
  Buffer.contents b

let underscore = Uchar.of_char '_'
let hyphen = Uchar.of_char '-'
let period = Uchar.of_char '.'
let is_white_space = Uucp.White.is_white_space
let is_alphabetic = Uucp.Alpha.is_alphabetic
let is_hex_digit = Uucp.Num.is_hex_digit

let slugify s =
  let s = trim_start_while (fun c -> not (is_alphabetic c)) s in
  let length = String.length s in
  let b = Buffer.create length in
  let last_is_ws = ref false in
  let add_to_buffer u =
    if !last_is_ws = true then begin
      Uutf.Buffer.add_utf_8 b (Uchar.of_char '-');
      last_is_ws := false
    end;
    Uutf.Buffer.add_utf_8 b u
  in
  let fold () _ = function
    | `Malformed _ -> add_to_buffer Uutf.u_rep
    | `Uchar u when is_white_space u && not !last_is_ws -> last_is_ws := true
    | `Uchar u when is_white_space u && !last_is_ws -> ()
    | `Uchar u ->
        (if is_alphabetic u || is_hex_digit u then
         match Uucp.Case.Map.to_lower u with
         | `Self -> add_to_buffer u
         | `Uchars us -> List.iter add_to_buffer us);
        if u = underscore || u = hyphen || u = period then add_to_buffer u
  in
  Uutf.String.fold_utf_8 fold () s;
  Buffer.contents b

let parse_inlines ~auto_identifiers (md, defs) : doc =
  let defs =
    let f (def : attributes Parser.link_def) =
      { def with label = Parser.normalize def.label }
    in
    List.map f defs
  in
  let identifiers = Identifiers.empty in
  let f identifiers = function
    | Ast_block.WithInline.Heading (attr, level, text) ->
        let attr, identifiers =
          if (not auto_identifiers) || List.mem_assoc "id" attr then
            (attr, identifiers)
          else
            let id = slugify (Ast_inline.to_plain_text text) in
            (* Default identifier if empty. It matches what pandoc does. *)
            let id = if id = "" then "section" else id in
            let count, identifiers = Identifiers.touch id identifiers in
            let id =
              if count = 0 then id else Printf.sprintf "%s-%i" id count
            in
            (("id", id) :: attr, identifiers)
        in
        (Ast_block.WithInline.Heading (attr, level, text), identifiers)
    | _ as c -> (c, identifiers)
  in
  List.fold_left
    (fun (accu, ids) src ->
      let dst, ids = src |> Ast_block.Mapper.map (parse_inline defs) |> f ids in
      (dst :: accu, ids))
    ([], identifiers)
    md
  |> fst
  |> List.rev

let escape_html_entities = Html.htmlentities

let of_channel ?(auto_identifiers = true) ic : doc =
  parse_inlines ~auto_identifiers (Block_parser.Pre.of_channel ic)

let of_string ?(auto_identifiers = true) s =
  parse_inlines ~auto_identifiers (Block_parser.Pre.of_string s)

let to_html doc = Html.to_string (Html.of_doc doc)
let to_sexp ast = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)

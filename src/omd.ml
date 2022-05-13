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

let txt ?(attrs = []) s = Text (attrs, s)

let em ?(attrs = []) il = Emph (attrs, il)

let strong ?(attrs = []) il = Strong (attrs, il)

let code ?(attrs = []) s = Code (attrs, s)

let hard_break = Hard_break []

let soft_break = Soft_break []

let link ?(attrs = []) li = Link (attrs, li)

let img ?(attrs = []) li = Image (attrs, li)

let html ?(attrs = []) s = Html (attrs, s)

let pg ?(attrs = []) il = Paragraph (attrs, il)

let ul ?(attrs = []) ?(spacing = Loose) l = List (attrs, Bullet '-', spacing, l)

let ol ?(attrs = []) ?(spacing = Loose) l =
  List (attrs, Ordered (1, '.'), spacing, l)

let blq ?(attrs = []) blocks = Blockquote (attrs, blocks)

let hr = Thematic_break []

let code_block ?(attrs = []) ~label s = Code_block (attrs, label, s)

let html_block ?(attrs = []) s = Html_block (attrs, s)

let def_list ?(attrs = []) l = Definition_list (attrs, l)

let of_channel ic = parse_inlines (Pre.of_channel ic)
let of_string s = parse_inlines (Pre.of_string s)
let to_html doc = Html.to_string (Html.of_doc doc)
let to_sexp ast = Format.asprintf "@[%a@]@." Sexp.print (Sexp.create ast)
let headers = Toc.headers
let toc = Toc.toc

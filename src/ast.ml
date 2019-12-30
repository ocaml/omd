type attributes =
  {
    a_id: string option;
    a_classes: string list;
    a_attributes: (string * string) list;
  }

let empty_attributes =
  {
    a_id = None;
    a_classes = [];
    a_attributes = [];
  }

type 'a link_def =
  {
    ld_label: 'a;
    ld_destination: string;
    ld_title: string option;
    ld_attributes: attributes;
  }

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

type code =
  {
    c_level: int;
    c_content: string;
    c_attributes: attributes;
  }

type link_kind =
  | Img
  | Url

type def_elt =
  {
    de_term: inline;
    de_defs: inline list;
  }

and def_list =
  {
    dl_content: def_elt list
  }

and link =
  {
    lk_kind: link_kind;
    lk_def: inline link_def;
  }

and ref =
  {
    rf_kind: link_kind;
    rf_label: inline;
    rf_def: string link_def;
  }

and tag =
  {
    tg_name: string;
    tg_content: inline;
    tg_attributes: attributes;
  }

and emph =
  {
    em_style: emph_style;
    em_kind: emph_kind;
    em_content: inline;
  }

and inline =
  | Concat of inline list
  | Text of string
  | Emph of emph
  | Code of code
  | Hard_break
  | Soft_break
  | Link of link
  | Ref of ref
  | Html of string
  | Tag of tag

type code_block_kind =
  | Tilde
  | Backtick

type block_list_kind =
  | Ordered of int * char
  | Unordered of char

type block_list_style =
  | Loose
  | Tight

type code_block =
  {
    cb_kind: code_block_kind option;
    cb_label: string option;
    cb_other: string option;
    cb_code: string option;
    cb_attributes: attributes;
  }

type heading =
  {
    h_level: int;
    h_text: inline;
    h_attributes: attributes;
  }

and tag_block =
  {
    tb_tag: string;
    tb_content: block list;
    tb_attributes: attributes;
  }

and block_list =
  {
    bl_kind: block_list_kind;
    bl_style: block_list_style;
    bl_blocks: block list list;
  }

and block =
  | Paragraph of inline
  | List of block_list
  | Blockquote of block list
  | Thematic_break
  | Heading of heading
  | Code_block of code_block
  | Html_block of string
  | Link_def of string link_def
  | Def_list of def_list
  | Tag_block of tag_block

let rec map f = function
  | Paragraph (Text x) -> Paragraph (f x)
  | Paragraph _ -> assert false
  | List l -> List  {l with bl_blocks = List.map (List.map (map f)) l.bl_blocks}
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading ({h_text = Text s; _} as h) -> Heading {h with h_text = f s}
  | Heading _ -> assert false
  | Def_list {dl_content} ->
      let dl_content =
        List.map (function
            | {de_term = Text s; de_defs} ->
                let de_defs =
                  List.map (function (Text s) -> f s | _ -> assert false) de_defs
                in
                {de_term = f s; de_defs}
            | _ -> assert false
          ) dl_content
      in
      Def_list {dl_content}
  | Tag_block t -> Tag_block {t with tb_content = List.map (map f) t.tb_content}
  | Code_block _ | Html_block _ | Link_def _ as x -> x

let defs ast =
  let rec loop acc = function
    | List l -> List.fold_left (List.fold_left loop) acc l.bl_blocks
    | Blockquote l | Tag_block {tb_content = l; _} -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Def_list _ | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

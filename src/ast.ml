module Impl = struct
  include Ast_inline
  include Ast_block.List_types
  include Ast_block.Table_alignments
  include Ast_block.WithInline

  type attributes = (string * string) list
  type doc = attributes block list

  let rec of_cst_block (blk : Cst.Impl.attributes Cst.Impl.block) :
      attributes block =
    match blk with
    | Cst.Impl.Paragraph (attr, inline) ->
        Paragraph (attr, Ast_inline.of_cst_inline inline)
    | Cst.Impl.List (attr, list_type, list_spacing, blk) ->
        List
          ( attr
          , list_type
          , list_spacing
          , blk |> List.map (List.map of_cst_block) )
    | Cst.Impl.Blockquote (attr, blk) ->
        Blockquote (attr, blk |> List.map of_cst_block)
    | Cst.Impl.Thematic_break atrr -> Thematic_break atrr
    | Cst.Impl.Heading (attr, _heading_type, level, inline) ->
        Heading (attr, level, Ast_inline.of_cst_inline inline)
    | Cst.Impl.Code_block (attr, s1, s2) -> Code_block (attr, s1, s2)
    | Cst.Impl.Html_block (atrr, s) -> Html_block (atrr, s)
    | Cst.Impl.Definition_list
        (attr, (def_list : Cst.Impl.attributes Cst.Impl.def_elt list)) ->
        let def_list : attributes def_elt list =
          def_list
          |> List.map
               (fun ({ term; defs } : Cst.Impl.attributes Cst.Impl.def_elt) ->
                 { term = Ast_inline.of_cst_inline term
                 ; defs = defs |> List.map Ast_inline.of_cst_inline
                 })
        in
        Definition_list (attr, def_list)
    | Cst.Impl.Table (attr, b_list, inline) ->
        let second =
          b_list
          |> List.map (fun (inline, cell) ->
                 (Ast_inline.of_cst_inline inline, cell))
        in
        let inline = inline |> List.map (List.map Ast_inline.of_cst_inline) in
        Table (attr, second, inline)
end

module type Intf = module type of Impl

module Util = struct
  include Impl

  let same_block_list_kind k1 k2 =
    match (k1, k2) with
    | Ordered (_, c1), Ordered (_, c2) | Bullet c1, Bullet c2 -> c1 = c2
    | _ -> false
end

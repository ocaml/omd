module Impl = struct
  include Cst_inline
  include Cst_block.List_types
  include Cst_block.Table_alignments
  include Cst_block.WithInline

  type attributes = (string * string) list
  type parse_tree = attributes block list
end

module type Intf = module type of Impl

module Util = struct
  include Impl

  let same_block_list_kind k1 k2 =
    match (k1, k2) with
    | Ordered (_, c1), Ordered (_, c2) | Bullet c1, Bullet c2 -> c1 = c2
    | _ -> false
end

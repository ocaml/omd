module Impl = struct
  include Ast_inline
  include Ast_block.List_types
  include Ast_block.WithInline

  type attributes = (string * string) list
  type doc = attributes block list
end

module type Intf = module type of Impl

module Util = struct
  include Impl

  let same_block_list_kind k1 k2 =
    match (k1, k2) with
    | Ordered (_, c1), Ordered (_, c2) | Bullet c1, Bullet c2 -> c1 = c2
    | _ -> false
end

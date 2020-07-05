module Pre : sig
  val of_channel: in_channel -> Ast.Raw.block list * Ast.link_def list
  val of_string: string -> Ast.Raw.block list * Ast.link_def list
end

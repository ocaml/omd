open Cst.Impl
module Raw = Cst_block.Raw

module Pre : sig
  val of_channel :
    in_channel -> attributes Raw.block list * attributes Parser.link_def list

  val of_string :
    string -> attributes Raw.block list * attributes Parser.link_def list
end

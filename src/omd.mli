(** A markdown parser in OCaml. *)

include module type of struct include Ast end

type t = block list
(** A markdown document *)

type printer = Html.printer =
  {
    document: printer       -> Buffer.t -> block list  -> unit;
    attributes: printer     -> Buffer.t -> attributes  -> unit;
    block: printer          -> Buffer.t -> block       -> unit;
    paragraph: printer      -> Buffer.t -> inline      -> unit;
    blockquote: printer     -> Buffer.t -> block list  -> unit;
    list: printer           -> Buffer.t -> block_list  -> unit;
    code_block: printer     -> Buffer.t -> code_block  -> unit;
    thematic_break: printer -> Buffer.t                -> unit;
    html_block: printer     -> Buffer.t -> string      -> unit;
    heading: printer        -> Buffer.t -> heading     -> unit;
    def_list: printer       -> Buffer.t -> def_list    -> unit;
    tag_block: printer      -> Buffer.t -> tag_block   -> unit;
    inline: printer         -> Buffer.t -> inline      -> unit;
    concat: printer         -> Buffer.t -> inline list -> unit;
    text: printer           -> Buffer.t -> string      -> unit;
    emph: printer           -> Buffer.t -> emph        -> unit;
    code: printer           -> Buffer.t -> code        -> unit;
    hard_break: printer     -> Buffer.t                -> unit;
    soft_break: printer     -> Buffer.t                -> unit;
    html: printer           -> Buffer.t -> string      -> unit;
    link: printer           -> Buffer.t -> link        -> unit;
    ref: printer            -> Buffer.t -> ref         -> unit;
    tag: printer            -> Buffer.t -> tag         -> unit;
  }

val of_channel: in_channel -> t

val of_string: string -> t

val default_printer: printer

val to_html: ?printer:printer -> t -> string

val to_sexp: t -> string

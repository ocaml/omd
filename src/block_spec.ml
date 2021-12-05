module Sub = Parser.Sub

type link_def = Ast.attributes Ast.link_def

type raw_block = Ast.attributes Ast.Raw.block

module Line = struct
  type t =
    { parsed : Parser.t
    ; slice : Sub.t
    }

  let v : string -> t =
   fun s ->
    let slice = Parser.Sub.of_string s in
    { parsed = Parser.parse slice; slice }

  let of_slice : Parser.Sub.t -> t =
   fun slice -> { slice; parsed = Parser.parse slice }

  let to_string { slice; _ } = Sub.to_string slice
end

module Block = struct
  type t =
    | Block of raw_block
    (* Used to represent list items and the document  *)
    | Block_list of raw_block list

  let v b = Block b

  let vs bs = Block_list bs

  (* FIXME rm *)
  let name_raw = function
    | Ast.Raw.Paragraph (_, _) -> "para"
    | Ast.Raw.List (_, _, _, _) -> "ls"
    | Ast.Raw.Blockquote (_, _) -> "bq"
    | Ast.Raw.Thematic_break _ -> "hr"
    | Ast.Raw.Heading (_, _, _) -> "hd"
    | Ast.Raw.Code_block (_, _, _) -> "code"
    | Ast.Raw.Html_block (_, _) -> "html"
    | Ast.Raw.Definition_list (_, _) -> "dl"

  let get_name = function
    | Block b -> name_raw b
    | Block_list bs -> List.map (fun b -> name_raw b) bs |> String.concat " "

  (** TODO replace with use of GADT *)
  let to_raw_block = function
    | Block b -> b
    | _ -> failwith "invalid use of to_raw_block"

  let to_raw_block_list = function
    | Block_list bs -> bs
    | _ -> failwith "invalid use of to_raw_block_list"
end

module rec Spec : sig
  (** A block of [module type Spec.T] specifies how to build a block from a line,
      and which [Spec.T]s can be children of the block. *)

  type 'a status =
    | Open of 'a * Line.t option
    | Close

  module type T = sig
    type t

    val kind : Kind.t

    val is_containable : bool
    (** [is_containable] is [true] if the specified block can be contained
        directly by container blocks, as opposed to requiring a special
        meta-container, like list items *)

    val name : string

    val of_line : Line.t -> (t * Line.t option) option
    (** Given a line, produce the remaining line data, and
        the block that is opened by it. *)

    val incorporate_line : Line.t -> t -> t option
    (** [incorporate_line line t] is [Some t'] when [line] can be incorporated
        into the block without opening any children, [t'] is updated block data
        incorporating [line], otherwise it is [None]. *)

    val remain_open : Line.t -> t -> t status
    (** [remain_open line] is [Open line'] when [line'] is the data left from
        [line] after consuming any tokens needed to keep the block open (e.g., a `>`
        for a blockquote) and the block should stay open, otherwise, if the line
        does not indicate that the block should stay open, it is [Close]. *)

    val to_block : children:Block.t list -> t -> Block.t
    (** [to_block ~children t] creates a new block based on any state accumulated in [t]
        and the children in [children]. *)
  end
end =
  Spec

and Kind : sig
  (** See https://spec.commonmark.org/0.30/#container-blocks

    > There are two basic kinds of container blocks: block quotes and list
      items. Lists are meta-containers for list items.  *)
  type t =
    | Leaf  (** A block that only contains inlines *)
    | Container  (** A block that can contains any other blocks *)
    | Meta_container of (module Spec.T)
        (** A block that contains a single, special class of blocks (e.g., lists
        contain list items) *)
end =
  Kind

module type Spec = Spec.T

(** Leaf blocks *)

module Paragraph : Spec = struct
  type t = string Stack.t

  let kind = Kind.Leaf

  let is_containable = true

  let name = "Paragraph"

  let empty = Stack.empty

  let of_line { Line.parsed; slice } =
    match parsed with
    | Parser.Lparagraph -> Some (empty |> Stack.push (Sub.to_string slice), None)
    | _ -> None

  let incorporate_line { Line.parsed; slice } t =
    match parsed with
    | Parser.Lparagraph -> Some (Stack.push (Sub.to_string slice) t)
    | _ -> None

  let remain_open { Line.parsed; _ } t =
    match parsed with
    | Parser.Lparagraph -> Spec.Open (t, None)
    | _ -> Spec.Close

  let to_block ~children:_ t =
    t |> Stack.to_list |> String.concat "\n" |> fun s ->
    Block.v (Ast.Raw.Paragraph ([], s))
end

(** Container blocks *)

module Blockquote : Spec = struct
  type t = unit

  let kind = Kind.Container

  let is_containable = true

  let name = "Blockquote"

  let of_line (line : Line.t) =
    match line.parsed with
    | Parser.Lblockquote s -> Some ((), Some (Line.of_slice s))
    | _ -> None

  let incorporate_line _ _ = None

  let remain_open (line : Line.t) t =
    match line.parsed with
    | Parser.Lblockquote s -> Spec.Open (t, Some (Line.of_slice s))
    | Parser.Lparagraph -> Spec.Open (t, Some line)
    | _ -> Spec.Close

  let to_block ~children () =
    let children = List.map Block.to_raw_block children in
    Block.v (Ast.Raw.Blockquote ([], children))
end

module ListItem : Spec = struct
  type t = unit

  let kind = Kind.Container

  let is_containable = false

  let name = "ListItem"

  let of_line (line : Line.t) =
    match line.parsed with
    | Parser.Llist_item (_, _, s) -> Some ((), Some (Line.of_slice s))
    | _ -> None

  let incorporate_line _ _ = None

  let remain_open (line : Line.t) t =
    match line.parsed with
    | Parser.Lparagraph -> Spec.Open (t, Some line)
    (* TODO Add support for indentation (unless that is more general) *)
    | _ -> Spec.Close

  let to_block ~children () =
    children |> List.map Block.to_raw_block |> Block.vs
end

(* Meta containers  *)
module ListBlock : Spec = struct
  type t =
    { list_type : Ast.list_type
    ; indent : int
    ; prev_empty : bool
    ; spacing : Ast.list_spacing
    }

  let kind = Kind.Meta_container (module ListItem)

  let is_containable = true

  let name = "ListBlock"

  let of_line (line : Line.t) =
    match line.parsed with
    | Parser.Llist_item (list_type, indent, _) ->
        (* Consume none of the line, since the the whole line is needed for the first child item *)
        Some
          ( { list_type; indent; spacing = Ast.Tight; prev_empty = false }
          , Some line )
    | _ -> None

  let incorporate_line _ _ = None

  (* TODO: Handle seeing empty lines and switching to spacing *)
  let remain_open (line : Line.t) t =
    match line.parsed with
    | Parser.Llist_item (list_type, _, _)
      when Ast.same_block_list_kind list_type t.list_type ->
        Spec.Open (t, Some line)
    | Parser.Lempty -> Spec.Open ({ t with prev_empty = true }, None)
    | _ -> Spec.Close

  let to_block ~children t =
    Logs.debug (fun f ->
        List.iter
          (fun b -> f "child of %s: %s" name (Block.get_name b))
          children);
    let children = List.map Block.to_raw_block_list children in
    Block.v (Ast.Raw.List ([], t.list_type, t.spacing, children))
end

(** TODO Removet his spec, obviated by division into leaf/container/meta_container block *)
module Document : Spec with type t = unit = struct
  type t = unit

  let kind = Kind.Container

  let is_containable = false

  let name = "Document"

  (* The Document block is not created from any line *)
  let of_line _ = None

  (* The Document block cannot incorporate any line *)
  let incorporate_line _ _ = None

  (* We only close this block manually *)
  (* TODO: Should Parser.t include an EOF constructor? Should this argument work on an option type?  *)
  let remain_open line t = Spec.Open (t, Some line)

  (** TODO Maybe we just never call `to_block` on this one node? *)
  let to_block ~children () = List.map Block.to_raw_block children |> Block.vs
end

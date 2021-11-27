(**
   {1 Algorithm}

    The parsing strategy is informed by https://spec.commonmark.org/0.30/#appendix-a-parsing-strategy
    Approach to extensibility informed by https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions

   Description of the general algorithm used for the block parser.

   {2 Data Structures}

   - [lines]: A stream of tokenized lines, encoding a markdown document
   - [module M : Spec]: A block specification, describing how to form a block out of lines.
   - [node]: A pair of a [module M : Spec] and a value [v : M.t], which is used to track
        any intermediating data needed to process the [lines]
   - [tree]: An in-progress representation of the document tree being parsed
        from out of the [lines], represented as a tree of [node]s with the following
        properties:
      - Each [node] has two different kinds of children in the tree:
      - Closed children: those children that have been fully parsed out of
                [lines]
      - A single open child, which is a [node]
      - There is a cursor into the tree, tracking the currently focused node,
              this lets us track the context within which a [line] from [lines] is
              being processed, and saves us some iterations throug through the tree.
   - [state]: A record tracking
      - The current [tree]
      - A list of reference link definitions

   In actuality, the live nodes are only ever along the currently growing
   branch of the document tree, and this live branch is represented by a
   zipper of nodes.

   {2 Process}

   Initialize the empty [state], with a [tree] containing only the empty
   [document] node and an empty list for the link definitions.

   Fold over the stream of lines with a function [parse : state ->
   line -> state] that works to incorporate the [line] into the [tree] as
   follows:

   1. Check the first token in [line].
   2. {b Continuation incorporation:} Ask the currently focused [node] whether
    the token indicates the line should be incorporated into its block. If so,
    incorporate the data from the line into the current node, and return the
    updated tree.  E.g., if the currently focused [node] is building a
    paragraph, a lazy continuation line will indicate incorporation.
   3. Otherwise, move the cursor to the first open node (in the root of the
    document).
   4. {b Block sustaining:} Ask the currently focused [node] whether the first
    token indicates that it should stay open.
   6. If so, then consume that token, advance the cursor to the open child
    of the current [node], and return to 4.
   7. {b Block closing:} If the token doesn't indicate that the focused [node]
    should remain open, then close the [node], creating a new block, and retract
    the cursor to focus on the current [node]'s parent.
   (TODO By this point we should be focused on the last open child.)
   8. {b Block opening:} If there are remaining tokens in the line, then check
    if the next token indicates a new block should be opened, then create a new
    node of the approriate kind, and add it to the current [node]. Then advance
    focus to that new node, and go to 8. (TODO Else close current and go back.)
   9. (TODO RM? I think this is done just by block opening logic...)
    After all tokens have been consumed, if any text remains, add it to
    the current [node]'s content.
   TODO: Add extraction of reference link defs
   10. Return the updated [tree]. *)

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
end

type _ block =
  | Block : raw_block block
  | Block_list : raw_block list block
  | Document : raw_block list block

module rec Spec : sig
  (** A block of [module type Spec.T] specifies how to build a block from a line,
      and which [Spec.T]s can be children of the block. *)

  type status =
    | Open of Line.t option
    | Close

  (* TODO Different signatures for container blocks and leafe blocks?
   * - 'Leaf block' `to_block` functions should be `t -> raw_block`, and they
   *   have no `child_blocks`. We should be able to easliy form a full
   *   `Spec` from a functor over a leaf speak.
   * - 'Container  blocks' can contain *any* children
   * - 'Meta Container' blocks can contain only the appropriate list type
   *
   *  The "full" block spec has a value `kind : kind` where `type kind = Leaf | Container | Meta_container of Spec.T list,
   *  the last being a list of the block specs the "Meta_conatiner" can hold. E.g., List items, for a list *)
  module type T = sig
    type t

    val name : string

    val empty : t

    val of_line : Line.t -> (t * Line.t option) option
    (** Given a line, produce the remaining line data, and
        the block that is opened by it. *)

    val incorporate_line : Line.t -> t -> t option
    (** [incorporate_line line t] is [Some t'] when [line] can be incorporated
        into the block without opening any children, [t'] is updated block data
        incorporating [line], otherwise it is [None]. *)

    val remain_open : Line.t -> t -> status
    (** [remain_open line] is [Open line'] when [line'] is the data left from
        [line] after consuming any tokens needed to keep the block open (e.g., a `>`
        for a blockquote) and the block should stay open, otherwise, if the line
        does not indicate that the block should stay open, it is [Close]. *)

    val to_block : children:raw_block list -> t -> raw_block
    (** [to_block ~children t] creates a new block based on any state accumulated in [t]
        and the children in [children]. *)

    val child_blocks : (module Spec.T) list
    (** [child_blocks] is a list of specs for the blocks that can be a child
        of the specified block. *)
  end
end =
  Spec

module type Spec = Spec.T
(* TODO move the blocks into `Spec` module?  *)

(** Leaf blocks *)

module Paragraph : Spec = struct
  type t = string Stack.t

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

  let remain_open { Line.parsed; _ } _ =
    match parsed with
    | Parser.Lparagraph -> Spec.Open None
    | _ -> Spec.Close

  let to_block ~children:_ t =
    t |> Stack.to_list |> String.concat "\n" |> fun s ->
    Ast.Raw.Paragraph ([], s)

  let child_blocks = []
end

(** Container blocks *)

module Blockquote : Spec = struct
  type t = unit

  let name = "Blockquote"

  let empty = ()

  let of_line (line : Line.t) =
    match line.parsed with
    | Parser.Lblockquote s -> Some ((), Some (Line.of_slice s))
    | _ -> None

  let incorporate_line _ _ = None

  let remain_open (line : Line.t) _ =
    match line.parsed with
    | Parser.Lblockquote s -> Spec.Open (Some (Line.of_slice s))
    | Parser.Lparagraph -> Spec.Open (Some line)
    | _ -> Spec.Close

  let to_block ~children () = Ast.Raw.Blockquote ([], children)

  (** TODO Turn this into "Container block" *)
  let child_blocks = raise (Failure "TODO: All?")
end

module List : Spec = struct
  type t =
    { list_type : Ast.list_type
    ; indent : int
    ; prev_empty : bool
    ; spacing : Ast.list_spacing
    }

  let name = "List"

  let empty =
    raise
      (Failure "TODO Remove this constructor? No such thing as empty list...")

  let of_line (line : Line.t) =
    match line.parsed with
    | Parser.Llist_item (list_type, indent, _) ->
        (* Consume none of the line, since the the whole line is needed for the first child item *)
        Some
          ( { list_type; indent; spacing = Ast.Tight; prev_empty = false }
          , Some line )
    | _ -> None

  let incorporate_line _ _ = None

  let remain_open (line : Line.t) t =
    match line.parsed with
    | Parser.Llist_item (list_type, _, _)
      when Ast.same_block_list_kind list_type t.list_type ->
        Spec.Open (Some line)
    | _ -> Spec.Close

  (** TODO FUCK! How do we deal with the fact that List's don't take normal blocks!!! *)
  let to_block ~children t =
    Ast.Raw.List ([], t.list_type, t.spacing, [children])

  let child_blocks = raise (Failure "TODO: Only accepts ListItems. Convert to Meta_container kind")
end

module Document : Spec = struct
  type t = unit

  let name = "Document"

  let empty = ()

  (* The Document block is not created from any line *)
  let of_line _ = None

  (* The Document block cannot incorporate any line *)
  let incorporate_line _ _ = None

  (* We only close this block manually *)
  (* TODO: Should Parser.t include an EOF constructor? Should this argument work on an option type?  *)
  let remain_open line _ = Spec.Open (Some line)

  (** TODO Maybe we just never call `to_block` on this one node? *)
  let to_block ~children:_ _ =
    failwith "TODO cannot return a block due to document model"

  let child_blocks : (module Spec.T) list = [ (module Blockquote) ]
end

module Tree = struct
  module Node = struct
    (* See https://discuss.ocaml.org/t/the-shape-design-problem/7810/8 *)
    type t = Node : (module Spec with type t = 'a) * 'a -> t

    let get_mod : t -> (module Spec) = fun (Node ((module M), _)) -> (module M)

    let get_val (type a) ((_, v) : (module Spec with type t = a) * a) = v

    let v (type a) (module M : Spec with type t = a) (x : a) =
      Node ((module M), x)

    let remain_open : t -> Line.t -> Spec.status =
     fun (Node ((module M), n)) line -> M.remain_open line n

    let close ~children : t -> raw_block =
     fun (Node ((module M), t)) -> M.to_block ~children t

    (** [new_child node line] is [Some (child_node, rest_of_line)] if the
        [child_node] can be created for [node] given the [line], and
        [rest_of_line] is whatever remaining data is parsed out of [line].  If
        no child can be crated, it is [None]. *)
    let add_child : t -> Line.t -> (t * Line.t option) option =
     fun node line ->
      let (Node ((module M), _)) = node in
      let rec find_child = function
        | [] -> None
        | (module S : Spec) :: specs ->
        match S.of_line line with
        | None -> find_child specs
        | Some (n, line') -> Some (v (module S) n, line')
      in
      find_child M.child_blocks

    let incorporate_line (Node ((module M), a) : t) line : t option =
      M.incorporate_line line a |> Option.map (v (module M))
  end

  let ( let+ ) o f = Option.map f o

  type node =
    { node : Node.t  (** A node processing an open block *)
    ; closed_children : raw_block Stack.t
    }

  type t =
    { parents : node Stack.t  (** The open parents of the current node *)
    ; current : node  (** The currently focused node *)
    ; children : node Stack.t  (** The open children of the current node *)
    }

  let back : t -> t option =
   fun { parents; current; children } ->
    let+ current', parents' = Stack.pop parents in
    { parents = parents'
    ; current = current'
    ; children = Stack.push current children
    }

  let rec rewind : t -> t =
   fun t ->
    match back t with
    | None -> t
    | Some t' -> rewind t'

  let fwd : t -> t option =
   fun { parents; current; children } ->
    let+ current', children' = Stack.pop children in
    { parents = Stack.push current parents
    ; current = current'
    ; children = children'
    }

  let rec ffwd : t -> t =
   fun t ->
    match fwd t with
    | None -> t
    | Some t' -> ffwd t'

  let empty =
    { parents = Stack.empty
    ; current =
        { node = Node ((module Document), Document.empty)
        ; closed_children = Stack.empty
        }
    ; children = Stack.empty
    }

  let add_new_child_of_current : Node.t -> t -> t =
   fun node t ->
    if not (Stack.is_empty t.children) then
      failwith "invalid child addition: not at last child"
    else
      { t with
        children = Stack.push { node; closed_children = Stack.empty } t.children
      }
      |> fwd
      |> Option.get

  let update_current_node : (Node.t -> Node.t option) -> t -> t =
   fun f t ->
    match f t.current.node with
    | None -> t
    | Some node -> { t with current = { t.current with node } }

  let add_closed_child_to_current : raw_block -> t -> t =
   fun child t ->
    let closed_children = Stack.push child t.current.closed_children in
    { t with current = { t.current with closed_children } }

  let close_current_node : t -> t =
   fun t ->
    let children = t.current.closed_children |> Stack.to_list in
    let closed_block = Node.close ~children t.current.node in
    match back t with
    | None -> failwith "invalid close of root node" (* FIXME *)
    | Some t' -> add_closed_child_to_current closed_block t'

  (** TODO *)
  let rec open_children_of_current : Line.t -> t -> t =
   fun line t ->
    match Node.add_child t.current.node line with
    (* Cannot add child to current, so close it, and add to parent *)
    | None -> t |> close_current_node |> open_children_of_current line
    (* Add the new child, and keep processing the line *)
    | Some (n, Some line') ->
        add_new_child_of_current n t |> open_children_of_current line'
    (* Add the last child, because we're out of line *)
    | Some (n, None) -> add_new_child_of_current n t
end

module State = struct
  type t =
    { link_defs : link_def list
    ; tree : Tree.t
    }

  let empty = { link_defs = []; tree = Tree.empty }
end

(* TODO Move into `Tree`: has no need of state? *)
(* TODO Ensure algorithm agrees with specification *)
(* TODO This should always return node at last child
 * - Add phantom type to track when node is at end? *)
let rec sustain_blocks : State.t -> Line.t -> Line.t option * State.t =
 fun state line ->
  match Tree.Node.remain_open state.tree.current.node line with
  | Spec.Close ->
      (* (7) Block closing *)
      (Some line, { state with tree = Tree.close_current_node state.tree })
  | Spec.Open None -> (None, state) (* No more line left for sustaining *)
  | Spec.Open (Some line') ->
  match Tree.fwd state.tree with
  | None -> (Some line', state) (* No more live nodes to sustain *)
  | Some tree -> sustain_blocks { state with tree } line'
(* Look to sustain further nodes *)

let parse : State.t -> Line.t -> State.t =
 fun state line ->
  (* (2) Coninuation incorporation *)
  match Tree.Node.incorporate_line state.tree.current.node line with
  | Some node ->
      let tree = Tree.update_current_node (Fun.const (Some node)) state.tree in
      { state with tree }
  | None ->
  (* (4) Block sustaining *)
  match sustain_blocks state line with
  | None, state' -> state'
  | Some line', state' ->
      (* (8) Block opening *)
      { state' with tree = Tree.open_children_of_current line' state'.tree }

let read_line : string -> int -> string * int option =
 fun s off ->
  let buf = Buffer.create 128 in
  let rec loop cr_read off =
    if off >= String.length s then
      (Buffer.contents buf, None)
    else
      match s.[off] with
      | '\n' -> (Buffer.contents buf, Some (succ off))
      | '\r' ->
          if cr_read then Buffer.add_char buf '\r';
          loop true (succ off)
      | c ->
          if cr_read then Buffer.add_char buf '\r';
          Buffer.add_char buf c;
          loop false (succ off)
  in
  loop false off

let seq_of_lines : string -> string Seq.t =
 fun s ->
  let rec seq offset () =
    match offset with
    | None -> Seq.Nil
    | Some off ->
        let line, off' = read_line s off in
        Seq.Cons (line, seq off')
  in
  seq (Some 0)

let of_string : string -> State.t =
 fun s -> seq_of_lines s |> Seq.map Line.v |> Seq.fold_left parse State.empty

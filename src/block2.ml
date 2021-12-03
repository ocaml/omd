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

(* TODO Add pps for these terms? *)
(* TODO: Make logging configurable at compile time! (By compiler with built in ppx?) *)

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
(* TODO move the blocks into `Spec` module?  *)

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
    Debug.log (fun f ->
        List.iter
          (fun b -> f "[log] child of %s: %s" name (Block.get_name b))
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

let blocks : (module Spec) list =
  [ (module Paragraph)
  ; (module Blockquote)
  ; (module ListItem)
  ; (module ListBlock)
  ; (module Document)
  ]

let containable = List.filter (fun (module M : Spec) -> M.is_containable) blocks

module Tree = struct
  module Node = struct
    (* See https://discuss.ocaml.org/t/the-shape-design-problem/7810/8 *)
    type t =
      | Node :
          { spec : (module Spec with type t = 'a)
          ; builder : 'a
          ; children : Block.t Stack.t
          }
          -> t

    let v :
        type a.
        ?children:Block.t Stack.t -> (module Spec with type t = a) -> a -> t =
     fun ?(children = Stack.empty) (module M) builder ->
      Node { spec = (module M); builder; children }

    let name : t -> string = fun (Node { spec = (module M); _ }) -> M.name

    let remain_open : t -> Line.t -> (t * Line.t option) option =
     fun (Node ({ spec = (module M); builder; _ } as n)) line ->
      match M.remain_open line builder with
      | Spec.Close -> None
      | Spec.Open (builder, line) -> Some (Node { n with builder }, line)

    let close : t -> Block.t =
     fun (Node { spec = (module M); builder; children }) ->
      Debug.log (fun f -> f "[log] closing %s\n" M.name);
      let children = Stack.to_list children in
      M.to_block ~children builder

    let add_child : Block.t -> t -> t =
     fun block (Node t) ->
      Node { t with children = Stack.push block t.children }

    let of_line_and_spec : Line.t -> (module Spec) -> (t * Line.t option) option
        =
     fun line (module M) ->
      match M.of_line line with
      | None -> None
      | Some (n, line') -> Some (v (module M) n, line')

    (** [open_child node line] is [Some (child_node, rest_of_line)] if the
        [child_node] can be created for [node] given the [line], and
        [rest_of_line] is whatever remaining data is parsed out of [line].  If
        no child can be crated, it is [None]. *)
    let open_child : t -> Line.t -> (t * Line.t option) option =
     fun node line ->
      let (Node { spec = (module M); _ }) = node in
      let rec find_child = function
        | [] -> None
        | spec :: specs ->
        match of_line_and_spec line spec with
        | None -> find_child specs
        | Some node -> Some node
      in
      match M.kind with
      | Kind.Leaf -> None
      | Kind.Container -> find_child containable
      | Kind.Meta_container child -> of_line_and_spec line child

    let incorporate_line (Node { spec = (module M); builder; children }) line :
        t option =
      match M.incorporate_line line builder with
      | None -> None
      | Some builder' ->
          Debug.log (fun f ->
              f
                "[log] Incorporating '%s' into block %s\n"
                (Line.to_string line)
                M.name);
          Some (v ~children (module M) builder')
  end

  let ( let+ ) o f = Option.map f o

  type t =
    { parents : Node.t Stack.t  (** The open parents of the current node *)
    ; current : Node.t  (** The currently focused node *)
    ; children : Node.t Stack.t  (** The open children of the current node *)
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

  let pop_current : t -> (Node.t * t) option =
   fun { parents; current; children } ->
    let+ parents', current', children' =
      (* Try to replace the current with the next child *)
      match Stack.pop children with
      | Some (current', children') -> Some (parents, current', children')
      | None ->
      (* Otherwise replce current with the prev parent *)
      match Stack.pop parents with
      | Some (current', parents') -> Some (parents', current', children)
      | None -> None
    in
    (current, { parents = parents'; current = current'; children = children' })

  let empty =
    { parents = Stack.empty
    ; current = Node.v (module Document) ()
    ; children = Stack.empty
    }

  let log_tree : t -> unit =
   fun t ->
    Debug.log (fun f ->
        f
          "[log] Tree: {parents: [%s]; current: %s; children: %s}\n"
          (Stack.map Node.name t.parents |> Stack.to_list |> String.concat ",")
          (Node.name t.current)
          (Stack.map Node.name t.children |> Stack.to_list |> String.concat ","))

  let add_new_child_of_current : Node.t -> t -> t =
   fun child t ->
    if not (Stack.is_empty t.children) then (
      log_tree t;
      failwith
        (Printf.sprintf
           "invalid child addition %s: not at last child"
           (Node.name child))
    ) else
      { t with children = Stack.push child t.children } |> fwd |> Option.get

  let update_current_node : (Node.t -> Node.t option) -> t -> t =
   fun f t ->
    match f t.current with
    | None -> t
    | Some current -> { t with current }

  let set_current : Node.t -> t -> t =
   fun n t -> update_current_node (Fun.const (Some n)) t

  (* It assumed that the list represented a lineage of blocks, with the youngest
     child first in the list and oldest parent last *)
  let close_lineage : Node.t list -> Block.t = function
    | [] -> failwith "invalid lineage"
    | n :: ns ->
        Debug.log (fun f ->
            f "[log] Closing lineage starting with %s\n" (Node.name n));
        List.fold_left
          (fun child parent -> Node.add_child child parent |> Node.close)
          (Node.close n)
          ns

  exception Invalid_document_close

  let close_current_node : t -> t =
   fun { parents; current; children } ->
    log_tree { parents; current; children };
    let child = Stack.push current children |> Stack.to_list |> close_lineage in
    match Stack.pop parents with
    | None -> raise Invalid_document_close
    | Some (current', parents') ->
        Debug.log (fun f ->
            f "[log] Current after close %s\n" (Node.name current'));
        { parents = parents'
        ; current = Node.add_child child current'
        ; children = Stack.empty
        }

  (** TODO *)
  let rec open_children_of_current : Line.t -> t -> t =
   fun line t ->
    match Node.open_child t.current line with
    (* Cannot add child to current, so close it, and add to parent *)
    | None ->
        Debug.log (fun f ->
            f
              "[log] Cannot open child of %s with line '%s'\n"
              (Node.name t.current)
              (Line.to_string line));
        t |> close_current_node |> open_children_of_current line
    (* Add the new child, and keep processing the line *)
    | Some (n, Some line') ->
        Debug.log (fun f ->
            f
              "[log] Opened child %s of %s\n"
              (Node.name n)
              (Node.name t.current));
        add_new_child_of_current n t |> open_children_of_current line'
    (* Add the last child, because we're out of line *)
    | Some (n, None) ->
        Debug.log (fun f ->
            f
              "[log] Adding final child %s to %s: line is exhausted\n"
              (Node.name n)
              (Node.name t.current));
        add_new_child_of_current n t

  let to_document t =
    Debug.log (fun f -> f "[log] Closing document block\n");
    (* TODO Make safe, by removing Option.get operation *)
    let { current; _ } = rewind t |> fwd |> Option.get |> close_current_node in
    Node.close current |> Block.to_raw_block_list
end

module State = struct
  type t =
    { link_defs : link_def list
    ; tree : Tree.t
    }

  let empty = { link_defs = []; tree = Tree.empty }

  (** TODO Handle link_defs *)
  let to_document t =
    Debug.log (fun f -> f "[log] >>> Converting to document\n");
    t.tree |> Tree.to_document
end

(* TODO Move into `Tree`: has no need of state? *)
(* TODO Ensure algorithm agrees with specification *)
(* TODO This should always return node at last child
 * - Add phantom type to track when node is at end? *)
let rec sustain_blocks : State.t -> Line.t -> Line.t option * State.t =
 fun state line ->
  match Tree.Node.remain_open state.tree.current line with
  | None ->
      Debug.log (fun f ->
          f
            "[log] Closing %s since line '%s' cannot sustain\n"
            (Tree.Node.name state.tree.current)
            (Line.to_string line));
      Tree.log_tree state.tree;
      (* (7) Block closing *)
      (Some line, { state with tree = Tree.close_current_node state.tree })
  | Some (node, line_opt) -> (
      let tree' = state.tree |> Tree.set_current node in
      match Tree.fwd tree' with
      | None ->
          (line_opt, { state with tree = tree' })
          (* No more live nodes to sustain *)
      | Some tree ->
      match line_opt with
      | None -> (None, { state with tree }) (* No more line ot process *)
      | Some line' -> sustain_blocks { state with tree } line')
(* Look to sustain further nodes *)

let parse : State.t -> Line.t -> State.t =
 fun state line ->
  (* (2) Coninuation incorporation *)
  match Tree.Node.incorporate_line state.tree.current line with
  | Some node ->
      let tree = Tree.update_current_node (Fun.const (Some node)) state.tree in
      { state with tree }
  | None -> (
      (* (4) Block sustaining *)
      let state = { state with tree = Tree.rewind state.tree } in
      Debug.log (fun f -> f "[log] rewind\n");
      Tree.log_tree state.tree;
      match sustain_blocks state line with
      | None, state' -> state'
      | Some line', state' ->
          (* (8) Block opening *)
          Debug.log (fun f ->
              f "[log] opening line: '%s'\n" (Line.to_string line'));
          { state' with tree = Tree.open_children_of_current line' state'.tree }
      )

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

let seq_of_string : string -> string Seq.t =
 fun s -> Seq.unfold (Option.map (read_line s)) (Some 0)

let seq_of_channel : in_channel -> string Seq.t =
 fun ic ->
  Seq.unfold
    (fun () ->
      try Some (input_line ic, ()) with
      | End_of_file -> None)
    ()

let parse_seq : string Seq.t -> raw_block list * link_def list =
 fun seq ->
  let doc =
    seq
    |> Seq.map Line.v
    |> Seq.fold_left parse State.empty
    |> State.to_document
  in
  (doc, [])

module Pre = struct
  let of_channel : in_channel -> raw_block list * link_def list =
   fun ic ->
    (* ([], []) *)
    seq_of_channel ic |> parse_seq

  let of_string : string -> raw_block list * link_def list =
   fun s -> seq_of_string s |> parse_seq
end

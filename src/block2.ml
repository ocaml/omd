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

open Block_spec

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
    (** [Node]s record blocks that are currently open and being constructed *)

    (** We use a GADT to associate a [module Spec], which species how to parse
        out a block builder and how to construct a [raw_block] from a builder,
        with the existential type of the builder value.

        See https://discuss.ocaml.org/t/the-shape-design-problem/7810/8 *)
    type t =
      | Node :
          { spec : (module Spec with type t = 'a)  (**  *)
          ; builder : 'a
                (** Tracks any state needed to construct the [raw_block] *)
          ; children : Block.t Stack.t
                (** The closed children of the block specified in the node *)
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
      Logs.debug (fun f -> f "closing %s" M.name);
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
          Logs.debug (fun f ->
              f "Incorporating '%s' into block %s" (Line.to_string line) M.name);
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
    Logs.debug (fun f ->
        f
          "Open blocks: {parents: [%s]; current: %s; children: [%s]}"
          (Stack.map Node.name t.parents |> Stack.to_list |> String.concat ", ")
          (Node.name t.current)
          (Stack.map Node.name t.children |> Stack.to_list |> String.concat ","))

  let add_new_child_of_current : Node.t -> t -> t =
   fun child t ->
    if not (Stack.is_empty t.children) then (
      (* We shouldn't ever introduce children inbetween already opened blocks *)
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

  (* It is assumed that the list represented a lineage of blocks, with the youngest
     child first in the list and oldest parent last *)
  let close_lineage : Node.t list -> Block.t = function
    | [] -> failwith "invalid lineage"
    | n :: ns ->
        Logs.debug (fun f -> f "Closing lineage starting with %s" (Node.name n));
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
        let t' =
          { parents = parents'
          ; current = Node.add_child child current'
          ; children = Stack.empty
          }
        in
        log_tree t';
        t'

  (** TODO *)
  let rec open_children_of_current : Line.t -> t -> t =
   fun line t ->
    match Node.open_child t.current line with
    (* Cannot add child to current, so close it, and add to parent *)
    | None ->
        Logs.debug (fun f ->
            f
              "Cannot open child of %s with line '%s'"
              (Node.name t.current)
              (Line.to_string line));
        t |> close_current_node |> open_children_of_current line
    (* Add the new child, and keep processing the line *)
    | Some (n, Some line') ->
        Logs.debug (fun f ->
            f "Opening child %s of %s" (Node.name n) (Node.name t.current));
        add_new_child_of_current n t |> open_children_of_current line'
    (* Add the last child, because we're out of line *)
    | Some (n, None) ->
        Logs.debug (fun f ->
            f
              "Opening final child %s of %s (line is exhausted)"
              (Node.name n)
              (Node.name t.current));
        add_new_child_of_current n t

  let to_document t =
    Logs.debug (fun f -> f "Closing document block");
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
    Logs.debug (fun f -> f "Converting to document");
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
      Logs.debug (fun f ->
          f
            "Closing %s since line '%s' cannot sustain"
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
  Logs.debug (fun f -> f "Parsing line: '%s'" (Line.to_string line));
  (* (2) Coninuation incorporation *)
  match Tree.Node.incorporate_line state.tree.current line with
  | Some node ->
      let tree = Tree.update_current_node (Fun.const (Some node)) state.tree in
      { state with tree }
  | None -> (
      (* (4) Block sustaining *)
      let state = { state with tree = Tree.rewind state.tree } in
      Tree.log_tree state.tree;
      match sustain_blocks state line with
      | None, state' -> state'
      | Some line', state' ->
          (* (8) Block opening *)
          Logs.debug (fun f ->
              f "Opening blocks with: '%s'" (Line.to_string line'));
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

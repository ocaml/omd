open Ast

let same_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Unordered c1, Unordered c2 -> c1 = c2
  | _ -> false

type 'a t = 'a block

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List (k, st, xs) -> List (k, st, List.map (List.map (map f)) xs)
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading (i, x) -> Heading (i, f x)
  | Code_block _ as x -> x
  | Html_block _ as x -> x
  | Link_def {label; destination; title} -> Link_def {label = f label; destination; title}

let defs ast =
  let rec loop acc = function
    | List (_, _, l) -> List.fold_left (List.fold_left loop) acc l
    | Blockquote l -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

module Pre = struct
  type block = string t

  type container =
    | Rblockquote of t
    | Rlist of list_kind * list_style * bool * int * block list list * t
    | Rparagraph of string list
    | Rfenced_code of int * int * fenced_code_kind * string * string list
    | Rindented_code of string list
    | Rhtml of Block_parser.html_kind * string list
    | Rempty

  and t =
    {
      blocks: block list;
      next: container;
    }

  let concat l = String.concat "\n" (List.rev l)

  let trim_left s =
    let rec loop i =
      if i >= String.length s then
        i
      else begin
        match s.[i] with
        | ' ' | '\t' ->
            loop (succ i)
        | _ ->
            i
      end
    in
    let i = loop 0 in
    if i > 0 then String.sub s i (String.length s - i) else s

  let rec close {blocks; next} =
    match next with
    | Rblockquote state ->
        Blockquote (finish state) :: blocks
    | Rlist (kind, style, _, _, closed_items, state) ->
        List (kind, style, List.rev (finish state :: closed_items)) :: blocks
    | Rparagraph l ->
        let s = concat (List.map trim_left l) in
        let defs, off = Inline_parser.link_def [] (Lexing.from_string s) in
        let s = String.sub s off (String.length s - off) |> String.trim in
        let blocks = List.fold_right (fun def blocks -> Link_def def :: blocks) defs blocks in
        if s = "" then blocks else Paragraph s :: blocks
    | Rfenced_code (_, _, kind, info, []) ->
        Code_block (Some (kind, info), None) :: blocks
    | Rfenced_code (_, _, kind, info, l) ->
        Code_block (Some (kind, info), Some (concat l)) :: blocks
    | Rindented_code l -> (* TODO: trim from the right *)
        let rec loop = function "" :: l -> loop l | _ as l -> l in
        Code_block (None, Some (concat (loop l))) :: blocks
    | Rhtml (_, l) ->
        Html_block (concat l) :: blocks
    | Rempty ->
        blocks

  and finish state =
    List.rev (close state)

  let empty =
    {blocks = []; next = Rempty}

  let rec process {blocks; next} s =
    match next, Block_parser.classify_line s with
    | Rempty, Block_parser.Lempty ->
        {blocks; next = Rempty}
    | Rempty, Lblockquote s ->
        {blocks; next = Rblockquote (process empty s)}
    | Rempty, Lthematic_break ->
        {blocks = Thematic_break :: blocks; next = Rempty}
    | Rempty, Lsetext_heading (2, n) when n >= 3 ->
        {blocks = Thematic_break :: blocks; next = Rempty}
    | Rempty, Latx_heading (n, s) ->
        {blocks = Heading (n, s) :: blocks; next = Rempty}
    | Rempty, Lfenced_code (ind, num, q, info) ->
        {blocks; next = Rfenced_code (ind, num, q, info, [])}
    | Rempty, Lhtml (_, kind) ->
        process {blocks; next = Rhtml (kind, [])} s
    | Rempty, Lindented_code s ->
        {blocks; next = Rindented_code [Sub.to_string s]}
    | Rempty, Llist_item (kind, indent, s) ->
        {blocks; next = Rlist (kind, Tight, false, indent, [], process empty s)}
    | Rempty, (Lsetext_heading _ | Lparagraph) ->
        {blocks; next = Rparagraph [Sub.to_string s]}
    | Rparagraph _, Llist_item ((Ordered (1, _) | Unordered _), _, s1) when not (Block_parser.is_empty s1) ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rparagraph _, (Lempty | Lblockquote _ | Lthematic_break
                    | Latx_heading _ | Lfenced_code _ | Lhtml (true, _)) ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rparagraph (_ :: _ as lines), Lsetext_heading (n, _) ->
        {blocks = Heading (n, String.trim (String.concat "\n" (List.rev lines))) :: blocks; next = Rempty}
    | Rparagraph lines, _ ->
        {blocks; next = Rparagraph (Sub.to_string s :: lines)}
    | Rfenced_code (_, num, q, _, _), Lfenced_code (_, num', q1, "") when num' >= num && q = q1 ->
        {blocks = close {blocks; next}; next = Rempty}
    | Rfenced_code (ind, num, q, info, lines), _ ->
        let s =
          let ind = min (Block_parser.indent s) ind in
          if ind > 0 then
            Sub.offset ind s
          else
            s
        in
        {blocks; next = Rfenced_code (ind, num, q, info, Sub.to_string s :: lines)}
    | Rindented_code lines, Lindented_code s ->
        {blocks; next = Rindented_code (Sub.to_string s :: lines)}
    | Rindented_code lines, Lempty ->
        let n = min (Block_parser.indent s) 4 in
        let s = Sub.offset n s in
        {blocks; next = Rindented_code (Sub.to_string s :: lines)}
    | Rindented_code _, _ ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rhtml (Hcontains l as k, lines), _ when List.exists (fun t -> Sub.contains t s) l ->
        {blocks = close {blocks; next = Rhtml (k, Sub.to_string s :: lines)}; next = Rempty}
    | Rhtml (Hblank, _), Lempty ->
        {blocks = close {blocks; next}; next = Rempty}
    | Rhtml (k, lines), _ ->
        {blocks; next = Rhtml (k, Sub.to_string s :: lines)}
    | Rblockquote state, Lblockquote s ->
        {blocks; next = Rblockquote (process state s)}
    | Rlist (kind, style, _, ind, items, state), Lempty ->
        {blocks; next = Rlist (kind, style, true, ind, items, process state s)}
    | Rlist (_, _, true, ind, _, {blocks = []; next = Rempty}), _ when Block_parser.indent s >= ind ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rlist (kind, style, prev_empty, ind, items, state), _ when Block_parser.indent s >= ind ->
        let s = Sub.offset ind s in
        let state = process state s in
        let style =
          let rec new_block = function
            | Rblockquote {blocks = []; next}
            | Rlist (_, _, _, _, _, {blocks = []; next}) -> new_block next
            | Rparagraph [_]
            | Rfenced_code (_, _, _, _, [])
            | Rindented_code [_]
            | Rhtml (_, [_]) -> true
            | _ -> false
          in
          if prev_empty && new_block state.next then
            Loose
          else
            style
        in
        {blocks; next = Rlist (kind, style, false, ind, items, state)}
    | Rlist (kind, style, prev_empty, _, items, state), Llist_item (kind', ind, s) when same_list_kind kind kind' ->
        let style = if prev_empty then Loose else style in
        {blocks; next = Rlist (kind, style, false, ind, finish state :: items, process empty s)}
    | (Rlist _ | Rblockquote _), _ ->
        let rec loop = function
          | Rlist (kind, style, prev_empty, ind, items, {blocks; next}) ->
              begin match loop next with
              | Some next ->
                  Some (Rlist (kind, style, prev_empty, ind, items, {blocks; next}))
              | None ->
                  None
              end
          | Rblockquote {blocks; next} ->
              begin match loop next with
              | Some next ->
                  Some (Rblockquote {blocks; next})
              | None ->
                  None
              end
          | Rparagraph (_ :: _ as lines) ->
              begin match Block_parser.classify_line s with
              | Block_parser.Lparagraph | Lindented_code _
              | Lsetext_heading (1, _) | Lhtml (false, _) ->
                  Some (Rparagraph (Sub.to_string s :: lines))
              | _ ->
                  None
              end
          | _ ->
              None
        in
        begin match loop next with
        | Some next ->
            {blocks; next}
        | None ->
            process {blocks = close {blocks; next}; next = Rempty} s
        end

  let process state s =
    process state (Sub.of_string s)

  let of_channel ic =
    let rec loop state =
      match input_line ic with
      | s ->
          loop (process state s)
      | exception End_of_file ->
          finish state
    in
    loop empty
end

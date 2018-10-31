open Ast

type blocks = string block list

type container =
  | Rblockquote of t
  | Rlist of list_kind * list_style * bool * int * blocks list * t
  | Rparagraph of string list
  | Rfenced_code of int * int * Auxlex.fenced_code_kind * string * string list
  | Rindented_code of string list
  | Rhtml of Auxlex.html_kind * string list
  | Rempty

and t =
  {
    blocks: blocks;
    next: container;
  }

let concat l = String.concat "\n" (List.rev l)

let rec close {blocks; next} =
  match next with
  | Rblockquote state ->
      Blockquote (finish state) :: blocks
  | Rlist (kind, style, _, _, closed_items, state) ->
      List (kind, style, List.rev (finish state :: closed_items)) :: blocks
  | Rparagraph l ->
      let s = concat (List.map String.trim l) in
      let defs, off = Htmllex.link_def [] (Lexing.from_string s) in
      let s = String.sub s off (String.length s - off) in
      let blocks = if String.trim s = "" then blocks else Paragraph s :: blocks in
      List.fold_right (fun def blocks -> Link_def def :: blocks) defs blocks
  | Rfenced_code (_, _, _, info, []) ->
      Code_block (info, None) :: blocks
  | Rfenced_code (_, _, _, info, l) ->
      Code_block (info, Some (concat l)) :: blocks
  | Rindented_code l -> (* TODO: trim from the right *)
      let rec loop = function "" :: l -> loop l | _ as l -> l in
      Code_block ("", Some (concat (loop l))) :: blocks
  | Rhtml (_, l) ->
      Html_block (concat l) :: blocks
  | Rempty ->
      blocks

and finish state =
  List.rev (close state)

let empty =
  {blocks = []; next = Rempty}

let rec process {blocks; next} s =
  match next, Auxlex.classify_line s with
  | Rempty, Auxlex.Lempty ->
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
  | Rparagraph _, (Lempty | Llist_item ((Ordered (1, _) | Unordered _), _, _) (* TODO non empty first line *)
                  | Lthematic_break | Latx_heading _ | Lfenced_code _ | Lhtml (true, _)) ->
      process {blocks = close {blocks; next}; next = Rempty} s
  | Rparagraph (_ :: _ as lines), Lsetext_heading (n, _) ->
      {blocks = Heading (n, String.trim (String.concat "\n" (List.rev lines))) :: blocks; next = Rempty}
  | Rparagraph lines, _ ->
      {blocks; next = Rparagraph (Sub.to_string s :: lines)}
  | Rfenced_code (_, num, q, _, _), Lfenced_code (_, num', q1, "") when num' >= num && q = q1 ->
      {blocks = close {blocks; next}; next = Rempty}
  | Rfenced_code (ind, num, q, info, lines), _ ->
      let s =
        let ind = min (Auxlex.indent s) ind in
        if ind > 0 then
          Sub.offset ind s
        else
          s
      in
      {blocks; next = Rfenced_code (ind, num, q, info, Sub.to_string s :: lines)}
  | Rindented_code lines, Lindented_code s ->
      {blocks; next = Rindented_code (Sub.to_string s :: lines)}
  | Rindented_code lines, Lempty ->
      let n = min (Auxlex.indent s) 4 in
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
  | Rlist (kind, style, prev_empty, ind, items, state), _ when Auxlex.indent s >= ind ->
      let s = Sub.offset ind s in
      let style =
        if prev_empty && state.next = Rempty && List.length state.blocks > 0 then
          Loose
        else
          style
      in
      {blocks; next = Rlist (kind, style, false, ind, items, process state s)}
  | Rlist (kind, style, prev_empty, _, items, state), Llist_item (kind', ind, s) when Ast.same_list_kind kind kind' ->
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
            begin match Auxlex.classify_line s with
            | Auxlex.Lparagraph | Lsetext_heading (1, _) | Lhtml (false, _) ->
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

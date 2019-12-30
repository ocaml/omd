open Ast

module Sub = Parser.Sub

let same_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Unordered c1, Unordered c2 -> c1 = c2
  | _ -> false

module Pre = struct
  type container =
    | Rblockquote of t
    | Rlist of block_list_kind * block_list_style * bool * int * block list list * t
    | Rparagraph of string list
    | Rfenced_code of int * int * code_block_kind * (string * string) * string list * attributes
    | Rindented_code of string list
    | Rhtml of Parser.html_kind * string list
    | Rdef_list of string * string list
    | Rtag of int * int * string * t * attributes
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
    | Rlist (bl_kind, bl_style, _, _, closed_items, state) ->
        List {bl_kind; bl_style; bl_blocks = List.rev (finish state :: closed_items)} :: blocks
    | Rparagraph l ->
        let s = concat (List.map trim_left l) in
        let defs, off = Parser.link_reference_definitions (Parser.P.of_string s) in
        let s = String.sub s off (String.length s - off) |> String.trim in
        let blocks = List.fold_right (fun def blocks -> Link_def def :: blocks) defs blocks in
        if s = "" then blocks else Paragraph (Text s) :: blocks
    | Rfenced_code (_, _, kind, (label, other), [], cb_attributes) ->
        Code_block {cb_kind = Some kind; cb_label = Some label; cb_other = Some other; cb_code = None; cb_attributes} :: blocks
    | Rfenced_code (_, _, kind, (label, other), l, cb_attributes) ->
        Code_block {cb_kind = Some kind; cb_label = Some label; cb_other = Some other; cb_code = Some (concat l); cb_attributes} :: blocks
    | Rdef_list (term, defs) ->
        let l, blocks =
          match blocks with
          | Def_list {dl_content} :: b -> dl_content, b
          | b -> [], b
        in
        Def_list {dl_content = l @ [{ de_term = Text term; de_defs = List.rev_map (fun s -> Text s) defs}]} :: blocks
    | Rtag (_, _, tb_tag, state, tb_attributes) ->
        Tag_block {tb_tag; tb_content = close state; tb_attributes} :: blocks
    | Rindented_code l -> (* TODO: trim from the right *)
        let rec loop = function "" :: l -> loop l | _ as l -> l in
        Code_block {cb_kind = None; cb_label = None; cb_other = None; cb_code = Some (concat (loop l)); cb_attributes = empty_attributes} :: blocks
    | Rhtml (_, l) ->
        Html_block (concat l) :: blocks
    | Rempty ->
        blocks

  and finish state =
    List.rev (close state)

  let empty =
    {blocks = []; next = Rempty}

  let classify_line s =
    Parser.parse s

  let rec process {blocks; next} s =
    match next, classify_line s with
    | Rempty, Parser.Lempty ->
        {blocks; next = Rempty}
    | Rempty, Lblockquote s ->
        {blocks; next = Rblockquote (process empty s)}
    | Rempty, Lthematic_break ->
        {blocks = Thematic_break :: blocks; next = Rempty}
    | Rempty, Lsetext_heading (2, n) when n >= 3 ->
        {blocks = Thematic_break :: blocks; next = Rempty}
    | Rempty, Latx_heading (h_level, text, h_attributes) ->
        {blocks = Heading {h_level; h_text = Text text; h_attributes} :: blocks; next = Rempty}
    | Rempty, Lfenced_code (ind, num, q, info, a) ->
        {blocks; next = Rfenced_code (ind, num, q, info, [], a)}
    | Rempty, Lhtml (_, kind) ->
        process {blocks; next = Rhtml (kind, [])} s
    | Rempty, Lindented_code s ->
        {blocks; next = Rindented_code [Sub.to_string s]}
    | Rempty, Llist_item (kind, indent, s) ->
        {blocks; next = Rlist (kind, Tight, false, indent, [], process empty s)}
    | Rempty, (Lsetext_heading _ | Lparagraph | Ldef_list _ | Ltag (_, _, "", _)) ->
        {blocks; next = Rparagraph [Sub.to_string s]}
    | Rparagraph [h], Ldef_list def ->
        {blocks; next = Rdef_list (h, [def])}
    | Rdef_list (term, defs), Ldef_list def ->
        {blocks; next = Rdef_list (term, def::defs)}
    | Rempty, Ltag (ind, n, tag, attributes) ->
        {blocks; next = Rtag (ind, n, tag, empty, attributes)}
    | Rparagraph _, Llist_item ((Ordered (1, _) | Unordered _), _, s1) when not (Parser.is_empty (Parser.P.of_string (Sub.to_string s1))) ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rparagraph _, (Lempty | Lblockquote _ | Lthematic_break
                    | Latx_heading _ | Lfenced_code _ | Lhtml (true, _)) ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rparagraph (_ :: _ as lines), Lsetext_heading (h_level, _) ->
        {blocks = Heading {h_level; h_text = Text (String.trim (String.concat "\n" (List.rev lines))); h_attributes = empty_attributes}:: blocks; next = Rempty}
    | Rparagraph lines, _ ->
        {blocks; next = Rparagraph (Sub.to_string s :: lines)}
    | Rfenced_code (_, num, q, _, _, _), Lfenced_code (_, num', q1, ("", _), _) when num' >= num && q = q1 ->
        {blocks = close {blocks; next}; next = Rempty}
    | Rfenced_code (ind, num, q, info, lines, a), _ ->
        let s =
          let ind = min (Parser.indent s) ind in
          if ind > 0 then
            Sub.offset ind s
          else
            s
        in
        {blocks; next = Rfenced_code (ind, num, q, info, Sub.to_string s :: lines, a)}
    | Rdef_list (term, d::defs), Lparagraph ->
        {blocks; next = Rdef_list (term, (d ^ "\n" ^ (Sub.to_string s))::defs)}
    | Rdef_list _, _ ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rtag (_, n, _, _, _), Ltag (_, n', "", _) when n' >= n ->
        {blocks = close {blocks; next}; next = Rempty}
    | Rtag (ind, n, tag, state, attributes), _ ->
        let s =
          let ind = min (Parser.indent s) ind in
          if ind > 0 then
            Sub.offset ind s
          else
            s
        in
        {blocks; next = Rtag (ind, n, tag, process state s, attributes)}
    | Rindented_code lines, Lindented_code s ->
        {blocks; next = Rindented_code (Sub.to_string s :: lines)}
    | Rindented_code lines, Lempty ->
        let n = min (Parser.indent s) 4 in
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
    | Rlist (_, _, true, ind, _, {blocks = []; next = Rempty}), _ when Parser.indent s >= ind ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rlist (kind, style, prev_empty, ind, items, state), _ when Parser.indent s >= ind ->
        let s = Sub.offset ind s in
        let state = process state s in
        let style =
          let rec new_block = function
            | Rblockquote {blocks = []; next}
            | Rlist (_, _, _, _, _, {blocks = []; next}) -> new_block next
            | Rparagraph [_]
            | Rfenced_code (_, _, _, _, [], _)
            | Rindented_code [_]
            | Rtag (_, _, _, _, _)
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
              begin match classify_line s with
              | Parser.Lparagraph | Lindented_code _
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

  let read_line s off =
    let buf = Buffer.create 128 in
    let rec loop cr_read off =
      if off >= String.length s then
        Buffer.contents buf, None
      else begin
        match s.[off] with
        | '\n' ->
            Buffer.contents buf, Some (succ off)
        | '\r' ->
            if cr_read then Buffer.add_char buf '\r';
            loop true (succ off)
        | c ->
            if cr_read then Buffer.add_char buf '\r';
            Buffer.add_char buf c;
            loop false (succ off)
      end
    in
    loop false off

  let of_string s =
    let rec loop state = function
      | None -> finish state
      | Some off ->
          let s, off = read_line s off in
          loop (process state s) off
    in
    loop empty (Some 0)
end

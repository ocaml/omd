module List_kind = struct
  type t = Auxlex.list_kind =
    | Ordered
    | Unordered
end

module List_style = struct
  type t =
    | Loose
    | Tight
end

type 'a t =
  | Paragraph of 'a
  | List of List_kind.t * List_style.t * 'a t list list
  | Blockquote of 'a t list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of string * string option
  | Html_block of string

let rec map ~f = function
  | Paragraph x -> Paragraph (f x)
  | List (k, st, xs) -> List (k, st, List.map (List.map (map ~f)) xs)
  | Blockquote xs -> Blockquote (List.map (map ~f) xs)
  | Thematic_break -> Thematic_break
  | Heading (i, x) -> Heading (i, f x)
  | Code_block _ as x -> x
  | Html_block _ as x -> x

module Parser = struct
  type block = string t
  type blocks = block list

  type container =
    | Rblockquote of t
    | Rlist of List_kind.t * List_style.t * bool * int * blocks list * t
    | Rparagraph of string list
    | Rfenced_code of int * int * string * string list
    | Rindented_code of string list
    | Rhtml of Auxlex.html_kind * string list
    | Rempty

  and t =
    {
      blocks: block list;
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
        Paragraph (String.trim (concat l)) :: blocks
    | Rfenced_code (_, _, info, []) ->
        Code_block (info, None) :: blocks
    | Rfenced_code (_, _, info, l) ->
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
    | Rempty, Lfenced_code (ind, num, info) ->
        {blocks; next = Rfenced_code (ind, num, info, [])}
    | Rempty, Lhtml (_, kind) ->
        process {blocks; next = Rhtml (kind, [])} s
    | Rempty, Lindented_code s ->
        {blocks; next = Rindented_code [Sub.to_string s]}
    | Rempty, Llist_item (kind, indent, s) ->
        {blocks; next = Rlist (kind, List_style.Tight, false, indent, [], process empty s)}
    | Rempty, (Lsetext_heading _ | Lparagraph _) ->
        {blocks; next = Rparagraph [Sub.to_string s]}
    | Rparagraph _, (Lempty | Llist_item _ (* TODO non empty first line *)
                    | Lthematic_break | Latx_heading _ | Lfenced_code _ | Lhtml (true, _)) ->
        process {blocks = close {blocks; next}; next = Rempty} s
    | Rparagraph (_ :: _ as lines), Lsetext_heading (n, _) ->
        {blocks = Heading (n, String.trim (String.concat "\n" (List.rev lines))) :: blocks; next = Rempty}
    | Rparagraph lines, _ ->
        {blocks; next = Rparagraph (Sub.to_string s :: lines)}
    | Rfenced_code (_, num, _, _), Lfenced_code (_, num', "") when num' >= num ->
        {blocks = close {blocks; next}; next = Rempty}
    | Rfenced_code (ind, num, info, lines), _ ->
        let s =
          let ind = min (Auxlex.indent s) ind in
          if ind > 0 then
            Sub.offset ind s
          else
            s
        in
        {blocks; next = Rfenced_code (ind, num, info, Sub.to_string s :: lines)}
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
            List_style.Loose
          else
            style
        in
        {blocks; next = Rlist (kind, style, false, ind, items, process state s)}
    | Rlist (kind, style, prev_empty, _, items, state), Llist_item (kind', ind, s) when kind = kind' ->
        let style = if prev_empty then List_style.Loose else style in
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
              | Auxlex.Lparagraph _ | Lsetext_heading (1, _) | Lhtml (false, _) ->
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
end

let to_html : 'a. (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a t -> unit = fun f b md ->
  let rec loop = function
    | Blockquote q ->
        Buffer.add_string b "<blockquote>\n";
        List.iter (fun md -> loop md; Buffer.add_char b '\n') q;
        Buffer.add_string b "</blockquote>"
    | Paragraph md ->
        Buffer.add_string b "<p>";
        f b md;
        Buffer.add_string b "</p>"
    | List (kind, style, l) ->
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "<ol>\n" | Unordered -> "<ul>\n");
        List.iter (fun x ->
            Buffer.add_string b "<li>";
            let _ = List.fold_left (li style) false x in
            Buffer.add_string b "</li>\n"
          ) l;
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "</ol>" | Unordered -> "</ul>")
    | Code_block ("", None) ->
        Buffer.add_string b "<pre><code></code></pre>"
    | Code_block (info, None) ->
        Buffer.add_string b (Printf.sprintf "<pre><code class=\"language-%s\"></code></pre>" info)
    | Code_block (lang, Some c) ->
        if lang = "" then
          Buffer.add_string b "<pre><code>"
        else
          Printf.bprintf b "<pre><code class=\"language-%s\">" lang;
        Buffer.add_string b (Utils.htmlentities ~md:false c);
        Buffer.add_string b "\n</code></pre>"
    | Thematic_break ->
        Buffer.add_string b "<hr />"
    | Html_block body ->
        Buffer.add_string b body
    | Heading (i, md) ->
        Buffer.add_string b (Printf.sprintf "<h%d>" i);
        f b md;
        Buffer.add_string b (Printf.sprintf "</h%d>" i)
  and li style prev_nl x =
    match x, style with
    | Paragraph md, List_style.Tight ->
        f b md;
        false
    | _ ->
        if not prev_nl then Buffer.add_char b '\n';
        loop x;
        Buffer.add_char b '\n';
        true
  in
  loop md

let to_html f mds =
  let b = Buffer.create 64 in
  List.iter (fun md -> to_html f b md; Buffer.add_char b '\n') mds;
  Buffer.contents b

let of_channel ic =
  let rec loop state =
    match input_line ic with
    | s ->
        loop (Parser.process state s)
    | exception End_of_file ->
        Parser.finish state
  in
  loop Parser.empty

module F = Format

let rec print f ppf = function
  | Paragraph x ->
      F.fprintf ppf "@[<1>(paragraph@ %a)@]" f x
  | List (_, _, xs) ->
      let pp ppf x = F.pp_print_list ~pp_sep:F.pp_print_space (print f) ppf x in
      F.fprintf ppf "@[<1>(list@ %a)@]" (F.pp_print_list ~pp_sep:F.pp_print_space pp) xs
  | Blockquote xs ->
      F.fprintf ppf "@[<1>(blockquote@ %a)@]"
        (F.pp_print_list ~pp_sep:F.pp_print_space (print f)) xs
  | Thematic_break ->
      F.pp_print_string ppf "thematic-break"
  | Heading (i, x) ->
      F.fprintf ppf "@[<1>(heading %d@ %a)@]" i f x
  | Code_block (lang, None) ->
      F.fprintf ppf "@[<1>(code:%s)@]" lang
  | Code_block (lang, Some x) ->
      F.fprintf ppf "@[<1>(code:%s %S)@]" lang x
  | Html_block x ->
      F.fprintf ppf "@[<1>(html %S)@]" x

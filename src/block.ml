module List_kind = struct
  type t =
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

  type html_kind =
    | Hcontains of string list
    | Hblank

  type container =
    | Rblockquote of blocks * container
    | Rlist of List_kind.t * List_style.t * bool * int * blocks list * blocks * container
    | Rparagraph of string list
    | Rfenced_code of int * int * string * string list
    | Rindented_code of string list
    | Rhtml of html_kind * string list
    | Rempty

  type t =
    {
      blocks: block list;
      next: container;
    }

  let concat l = String.concat "\n" (List.rev l)

  let rec close {blocks; next} =
    match next with
    | Rblockquote (closed, next) ->
        Blockquote (finish {blocks = closed; next}) :: blocks
    | Rlist (kind, style, _, _, closed_items, last_item, next) ->
        List (kind, style, List.rev (finish {blocks = last_item; next} :: closed_items)) :: blocks
    | Rparagraph l ->
        Paragraph (concat l) :: blocks
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

  let string_contains s1 s =
    let rec loop i =
      if i + String.length s1 > String.length s then
        false
      else
        s1 = String.sub s i (String.length s1) || loop (i + 1)
    in
    loop 0

  type line_kind =
    | Lempty
    | Lblockquote of string
    | Lthematic_break
    | Latx_heading of int * string
    | Lsetext_heading of int
    | Lfenced_code of int * int * string
    | Lindented_code of string
    | Lhtml of bool * html_kind
    | Llist_item of List_kind.t * int * string
    | Lparagraph of string

  let subn n s =
    let rec loop n i =
      if n = 0 || i >= String.length s then
        if i = 0 then
          s
        else
          String.sub s i (String.length s - i)
      else begin
        match s.[i] with
        | '\t' ->
            let ts = (i / 4 + 1) * 4 - i in
            if n >= ts then
              loop (n - ts) (i + 1)
            else
              let b = Buffer.create (String.length s) in
              for _ = 1 to ts - n do Buffer.add_char b ' ' done;
              Buffer.add_substring b s (i + 1) (String.length s - i - 1);
              Buffer.contents b
        | _ ->
            loop (n - 1) (i + 1)
        (* | _ -> *)
        (*     String.sub s i (String.length s - i) *)
      end
    in
    loop n 0

  let classify_line s =
    if Auxlex.is_empty s then
      Lempty
    else begin
      match Auxlex.is_blockquote s with
      | Some n ->
          let s = subn n s in
          Lblockquote s
      | None ->
          begin match Auxlex.is_setext_underline s with
          | Some n ->
              Lsetext_heading n
          | None ->
              begin match Auxlex.is_thematic_break s with
              | true ->
                  Lthematic_break
              | false ->
                  begin match Auxlex.is_atx_heading s with
                  | Some (n, s) ->
                      Latx_heading (n, s)
                  | None ->
                      begin match Auxlex.is_fenced_code s with
                      | Some (ind, num, info) ->
                          Lfenced_code (ind, num, info)
                      | None ->
                          begin match Auxlex.is_html_opening s with
                          | Some (can_interrupt_par, kind) ->
                              let kind =
                                match kind with
                                | `Contains l -> Hcontains l
                                | `Blank -> Hblank
                              in
                              Lhtml (can_interrupt_par, kind)
                          | None ->
                              if Auxlex.indent s >= 4 then
                                let s = subn 4 s in
                                Lindented_code s
                              else begin
                                match Auxlex.is_list_item s with
                                | Some (kind, indent) ->
                                    let kind =
                                      match kind with
                                      | Bullet _ -> List_kind.Unordered
                                      | Ordered _ -> Ordered
                                    in
                                    let s = subn indent s in
                                    Llist_item (kind, indent, s)
                                | None ->
                                    Lparagraph s
                              end
                          end
                      end
                  end
              end
          end
    end

  let process {blocks; next} s =
    let rec process blocks next s =
      match next, classify_line s with
      | Rempty, Lempty ->
          {blocks; next = Rempty}
      | Rempty, Lblockquote s ->
          let {blocks = c1; next} = process [] Rempty s in
          {blocks; next = Rblockquote (c1, next)}
      | Rempty, (Lthematic_break | Lsetext_heading 2) ->
          {blocks = Thematic_break :: blocks; next = Rempty}
      | Rempty, Latx_heading (n, s) ->
          {blocks = Heading (n, s) :: blocks; next = Rempty}
      | Rempty, Lfenced_code (ind, num, info) ->
          {blocks; next = Rfenced_code (ind, num, info, [])}
      | Rempty, Lhtml (_, kind) ->
          process blocks (Rhtml (kind, [])) s
      | Rempty, Lindented_code s ->
          {blocks; next = Rindented_code [s]}
      | Rempty, Llist_item (kind, indent, s) ->
          let {blocks = c1; next} = process [] Rempty s in
          {blocks; next = Rlist (kind, List_style.Tight, false, indent, [], c1, next)}
      | Rempty, (Lsetext_heading _ | Lparagraph _) ->
          {blocks; next = Rparagraph [s]}
      | Rparagraph _ as self, (Lempty | Lthematic_break | Latx_heading _ | Lfenced_code _ | Lhtml (true, _)) ->
          process (close {blocks; next = self}) Rempty s
      | Rparagraph (_ :: _ as lines), Lsetext_heading n ->
          {blocks = Heading (n, String.trim (String.concat "\n" (List.rev lines))) :: blocks; next = Rempty}
      | Rparagraph lines, _ ->
          {blocks; next = Rparagraph (s :: lines)}
      | Rfenced_code (_, num, _, _) as self, Lfenced_code (_, num', "") when num' >= num ->
          {blocks = close {blocks; next = self}; next = Rempty}
      | Rfenced_code (ind, num, info, lines), _ ->
          let s =
            let ind = min (Auxlex.indent s) ind in
            if ind > 0 then
              subn ind s
            else
              s
          in
          {blocks; next = Rfenced_code (ind, num, info, s :: lines)}
      | Rindented_code lines, Lindented_code s ->
          {blocks; next = Rindented_code (s :: lines)}
      | Rindented_code lines, Lempty ->
          let n = min (Auxlex.indent s) 4 in
          let s = subn n s in
          {blocks; next = Rindented_code (s :: lines)}
      | Rindented_code _ as self, _ ->
          process (close {blocks; next = self}) Rempty s
      | Rhtml (Hcontains l as k, lines), _ when List.exists (fun t -> string_contains t s) l ->
          {blocks = close {blocks; next = Rhtml (k, s :: lines)}; next = Rempty}
      | Rhtml (Hblank, _) as self, Lempty ->
          {blocks = close {blocks; next = self}; next = Rempty}
      | Rhtml (k, lines), _ ->
          {blocks; next = Rhtml (k, s :: lines)}
      | Rblockquote (c1, next), Lblockquote s ->
          let {blocks = c1; next} = process c1 next s in
          {blocks; next = Rblockquote (c1, next)}
      | Rlist (kind, style, prev_empty, _, items, c1, next), Llist_item (kind', ind, s) when kind = kind' ->
          let c1 = close {blocks = c1; next} in
          let {blocks = c2; next} = process [] Rempty s in
          {blocks; next = Rlist (kind, (if prev_empty then Loose else style), false, ind, List.rev c1 :: items, c2, next)}
      | Rlist (kind, style, _, ind, items, c1, next), Lempty ->
          let {blocks = c1; next} = process c1 next s in
          {blocks; next = Rlist (kind, style, true, ind, items, c1, next)}
      | Rlist (kind, style, prev_empty, ind, items, c1, next), _ when Auxlex.indent s >= ind ->
          let s = subn ind s in
          let style = if prev_empty && next = Rempty && List.length c1 > 0 then List_style.Loose else style in
          let {blocks = c1; next} = process c1 next s in
          {blocks; next = Rlist (kind, style, false, ind, items, c1, next)}
      | (Rlist _ | Rblockquote _ as self), _ ->
          let rec loop = function
            | Rlist (kind, style, prev_empty, ind, items, c, next) ->
                begin match loop next with
                | Some next ->
                    Some (Rlist (kind, style, prev_empty, ind, items, c, next))
                | None ->
                    None
                end
            | Rblockquote (c, next) ->
                begin match loop next with
                | Some next ->
                    Some (Rblockquote (c, next))
                | None ->
                    None
                end
            | Rparagraph (_ :: _ as lines) ->
                begin match classify_line s with
                | Lparagraph _ | Lsetext_heading 1 | Lhtml (false, _) ->
                    Some (Rparagraph (s :: lines))
                | _ ->
                    None
                end
            | _ ->
                None
          in
          begin match loop self with
          | Some next ->
              {blocks; next}
          | None ->
              process (close {blocks; next = self}) Rempty s
          end
    in
    process blocks next s
end

let to_html : 'a. ('a -> string) -> Buffer.t -> 'a t -> unit = fun f b md ->
  let rec loop = function
    | Blockquote q ->
        Buffer.add_string b "<blockquote>\n";
        List.iter (fun md -> loop md; Buffer.add_char b '\n') q;
        Buffer.add_string b "</blockquote>"
    | Paragraph md ->
        Buffer.add_string b "<p>";
        Buffer.add_string b (f md);
        Buffer.add_string b "</p>"
    | List (kind, style, l) ->
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "<ol>\n" | Unordered -> "<ul>\n");
        List.iter (fun x ->
            Buffer.add_string b "<li>";
            List.iteri (fun i md -> li (i = 0) style md) x;
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
        let md = f md in
        Buffer.add_string b (Printf.sprintf "<h%d>" i);
        Buffer.add_string b md;
        Buffer.add_string b (Printf.sprintf "</h%d>" i)
  and li first style x =
    match x, style with
    | Paragraph md, List_style.Tight ->
        Buffer.add_string b (f md)
    | _ ->
        if first then Buffer.add_char b '\n';
        loop x;
        Buffer.add_char b '\n'
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

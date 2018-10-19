module List_kind = struct
  type t =
    | Ordered
    | Unordered
end

type 'a t =
  | Paragraph of 'a
  | List of List_kind.t * 'a t list list
  | Blockquote of 'a t list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of string * string option
  | Html_block of string

let rec map ~f = function
  | Paragraph x -> Paragraph (f x)
  | List (k, xs) -> List (k, List.map (List.map (map ~f)) xs)
  | Blockquote xs -> Blockquote (List.map (map ~f) xs)
  | Thematic_break -> Thematic_break
  | Heading (i, x) -> Heading (i, f x)
  | Code_block _ as x -> x
  | Html_block _ as x -> x

module Parser = struct
  type blocks = string t list

  type html_kind =
    | Hcontains of string list
    | Hblank

  type container =
    | Rblockquote of blocks * container
    | Rlist of List_kind.t * int * blocks list * blocks * container
    | Rparagraph of string list
    | Rfenced_code of int * int * string * string list
    | Rindented_code of string list
    | Rhtml of html_kind * string list
    | Rempty

  type t =
    | Rdocument of blocks * container

  let close c next : blocks =
    let concat l = String.concat "\n" (List.rev l) in
    let rec close c = function
      | Rblockquote (closed, next) ->
          Blockquote (List.rev (close closed next)) :: c
      | Rlist (kind, _, closed_items, last_item, next) ->
          List (kind, List.rev (List.rev (close last_item next) :: closed_items)) :: c
      | Rparagraph l ->
          Paragraph (concat l) :: c
      | Rfenced_code (_, _, info, []) ->
          Code_block (info, None) :: c
      | Rfenced_code (_, _, info, l) ->
          Code_block (info, Some (concat l)) :: c
      | Rindented_code l ->
          Code_block ("", Some (concat l)) :: c
      | Rhtml (_, l) ->
          Html_block (concat l) :: c
      | Rempty ->
          c
    in
    close c next

  let finish (Rdocument (closed, next)) =
    List.rev (close closed next)

  let empty =
    Rdocument ([], Rempty)

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

  let classify_line s =
    if Auxlex.is_empty s then
      Lempty
    else begin
      match Auxlex.is_blockquote s with
      | Some n ->
          let s = String.sub s n (String.length s - n) in
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
                                (* FIXME handle tab *)
                                let s = String.sub s 4 (String.length s - 4) in
                                Lindented_code s
                              else begin
                                match Auxlex.is_list_item s with
                                | Some (kind, indent) ->
                                    let kind =
                                      match kind with
                                      | Bullet _ -> List_kind.Unordered
                                      | Ordered _ -> Ordered
                                    in
                                    let s = String.sub s indent (String.length s - indent) in
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

  let process (Rdocument (c, next)) s =
    let rec process c next s =
      match next, classify_line s with
      | Rempty, Lempty ->
          c, Rempty
      | Rempty, Lblockquote s ->
          let c1, next = process [] Rempty s in
          c, Rblockquote (c1, next)
      | Rempty, (Lthematic_break | Lsetext_heading 2) ->
          Thematic_break :: c, Rempty
      | Rempty, Latx_heading (n, s) ->
          Heading (n, s) :: c, Rempty
      | Rempty, Lfenced_code (ind, num, info) ->
          c, Rfenced_code (ind, num, info, [])
      | Rempty, Lhtml (_, kind) ->
          process c (Rhtml (kind, [])) s
      | Rempty, Lindented_code s ->
          c, Rindented_code [s]
      | Rempty, Llist_item (kind, indent, s) ->
          let c1, next = process [] Rempty s in
          c, Rlist (kind, indent, [], c1, next)
      | Rempty, (Lsetext_heading _ | Lparagraph _) ->
          c, Rparagraph [s]
      | Rparagraph _ as self, (Lempty | Lthematic_break | Latx_heading _ | Lfenced_code _ | Lhtml (true, _)) ->
          process (close c self) Rempty s
      | Rparagraph (_ :: _ as lines), Lsetext_heading n ->
          Heading (n, String.trim (String.concat "\n" (List.rev lines))) :: c, Rempty
      | Rparagraph lines, _ ->
          c, Rparagraph (s :: lines)
      | Rfenced_code (_, num, _, _) as self, Lfenced_code (_, num', "") when num' >= num ->
          close c self, Rempty
      | Rfenced_code (ind, num, info, lines), _ ->
          let s =
            let ind = min (Auxlex.indent s) ind in
            if ind > 0 then
              String.sub s ind (String.length s - ind)
            else
              s
          in
          c, Rfenced_code (ind, num, info, s :: lines)
      | Rindented_code lines, Lindented_code s ->
          c, Rindented_code (s :: lines)
      | Rindented_code lines, Lempty ->
          let n = min (Auxlex.indent s) 4 in
          let s = String.sub s n (String.length s - n) in
          c, Rindented_code (s :: lines)
      | Rindented_code _ as self, _ ->
          process (close c self) Rempty s
      | Rhtml (Hcontains l as k, lines), _ when List.exists (fun t -> string_contains t s) l ->
          close c (Rhtml (k, s :: lines)), Rempty
      | Rhtml (Hblank, _) as self, Lempty ->
          close c self, Rempty
      | Rhtml (k, lines), _ ->
          c, Rhtml (k, s :: lines)
      | Rblockquote (c1, next), Lblockquote s ->
          let c1, next = process c1 next s in
          c, Rblockquote (c1, next)
      | Rlist (kind, _, items, c1, next), Llist_item (kind', ind, s) when kind = kind' ->
          (* TODO handle loose lists *)
          let c1 = close c1 next in
          let c2, next = process [] Rempty s in
          c, Rlist (kind, ind, List.rev c1 :: items, c2, next)
      | Rlist (kind, ind, items, c1, next), Lempty ->
          let c1, next = process c1 next s in
          c, Rlist (kind, ind, items, c1, next)
      | Rlist (kind, ind, items, c1, next), _ when Auxlex.indent s >= ind ->
          let s = String.sub s ind (String.length s - ind) in
          let c1, next = process c1 next s in
          c, Rlist (kind, ind, items, c1, next)
      | (Rlist _ | Rblockquote _ as self), _ ->
          let rec loop = function
            | Rlist (kind, ind, items, c, next) ->
                begin match loop next with
                | Some next ->
                    Some (Rlist (kind, ind, items, c, next))
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
                | Lparagraph _ | Lsetext_heading _ | Lhtml (false, _) ->
                    Some (Rparagraph (s :: lines))
                | _ ->
                    None
                end
            | _ ->
                None
          in
          begin match loop self with
          | Some next ->
              c, next
          | None ->
              process (close c self) Rempty s
          end
    in
    let c, next = process c next s in
    Rdocument (c, next)
end

let to_html : 'a. ('a -> string) -> 'a t -> string = fun f md ->
  let b = Buffer.create 64 in
  let rec loop = function
    | Blockquote q ->
        Buffer.add_string b "<blockquote>\n";
        List.iter loop q;
        Buffer.add_string b "</blockquote>\n"
    | Paragraph md ->
        Buffer.add_string b "<p>";
        Buffer.add_string b (f md);
        Buffer.add_string b "</p>\n"
    | List (kind, l) ->
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "<ol>\n" | Unordered -> "<ul>\n");
        List.iter (fun x ->
            Buffer.add_string b "<li>";
            List.iter (li true) x;
            Buffer.add_string b "</li>\n"
          ) l;
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "</ol>\n" | Unordered -> "</ul>\n")
    | Code_block ("", None) ->
        Buffer.add_string b "<pre><code></code></pre>\n"
    | Code_block (info, None) ->
        Buffer.add_string b (Printf.sprintf "<pre><code class=\"language-%s\"></code></pre>\n" info)
    | Code_block (lang, Some c) ->
        if lang = "" then
          Buffer.add_string b "<pre><code>"
        else
          Printf.bprintf b "<pre><code class=\"language-%s\">" lang;
        Buffer.add_string b (Utils.htmlentities ~md:false c);
        Buffer.add_string b "\n</code></pre>\n"
    | Thematic_break ->
        Buffer.add_string b "<hr />\n"
    | Html_block body ->
        Buffer.add_string b body;
        Buffer.add_char b '\n'
    | Heading (i, md) ->
        let md = f md in
        Buffer.add_string b (Printf.sprintf "<h%d>" i);
        Buffer.add_string b md;
        Buffer.add_string b (Printf.sprintf "</h%d>\n" i)
  and li tight x =
    match x with
    | Paragraph md when tight ->
        Buffer.add_string b (f md)
    | _ ->
        Buffer.add_char b '\n';
        loop x
  in
  loop md;
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
  | List (_, xs) ->
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

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
  | Atx_heading of int * 'a
  | Code_block of string * string
  | Html_block of string

let rec map ~f = function
  | Paragraph x -> Paragraph (f x)
  | List (k, xs) -> List (k, List.map (List.map (map ~f)) xs)
  | Blockquote xs -> Blockquote (List.map (map ~f) xs)
  | Thematic_break -> Thematic_break
  | Atx_heading (i, x) -> Atx_heading (i, f x)
  | Code_block _ as x -> x
  | Html_block _ as x -> x

module Parser = struct
  type blocks = string t list

  type html_kind =
    | Hcomment

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
      | Rfenced_code (_, _, info, l) ->
          Code_block (info, concat l) :: c
      | Rindented_code l ->
          Code_block ("", concat l) :: c
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
    | Lfenced_code of int * int * string
    | Lindented_code of string
    | Lhtml of html_kind
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
                      | Some kind ->
                          let kind = match kind with `Comment -> Hcomment in
                          Lhtml kind
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

  let process (Rdocument (c, next)) s =
    let rec process c next s =
      match next, classify_line s with
      | Rempty, Lempty ->
          c, Rempty
      | Rempty, Lblockquote s ->
          let c1, next = process [] Rempty s in
          c, Rblockquote (c1, next)
      | Rempty, Lthematic_break ->
          Thematic_break :: c, Rempty
      | Rempty, Latx_heading (n, s) ->
          Atx_heading (n, s) :: c, Rempty
      | Rempty, Lfenced_code (ind, num, info) ->
          c, Rfenced_code (ind, num, info, [])
      | Rempty, Lhtml kind ->
          process c (Rhtml (kind, [])) s
      | Rempty, Lindented_code s ->
          c, Rindented_code [s]
      | Rempty, Llist_item (kind, indent, s) ->
          let c1, next = process [] Rempty s in
          c, Rlist (kind, indent, [], c1, next)
      | Rempty, Lparagraph s ->
          c, Rparagraph [s]
      | Rparagraph _ as self, (Lempty | Lthematic_break | Latx_heading _ | Lfenced_code _) ->
          process (close c self) Rempty s
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
      | Rindented_code _ as self, _ ->
          process (close c self) Rempty s
      | Rhtml (Hcomment, lines), _ when string_contains "-->" s ->
          close c (Rhtml (Hcomment, s :: lines)), Rempty
      | Rhtml (Hcomment, lines), _ ->
          c, Rhtml (Hcomment, s :: lines)
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
                | Lparagraph s (* other kinds that cannot interrupt paragraph *) ->
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
  let rec loop ~p indent = function
    | Blockquote q ->
        Buffer.add_string b "<blockquote>";
        List.iter (loop ~p:true indent) q;
        Buffer.add_string b "</blockquote>"
    | Paragraph md ->
        if p then Buffer.add_string b "<p>";
        Buffer.add_string b (f md);
        if p then Buffer.add_string b "</p>\n"
    | List (kind, l) ->
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "<ol>\n" | Unordered -> "<ul>\n");
        List.iter (fun li ->
            Buffer.add_string b "<li>";
            List.iter (loop ~p:false (indent + 2)) li;
            Buffer.add_string b "</li>\n"
          ) l;
        Buffer.add_string b
          (match kind with List_kind.Ordered -> "</ol>\n" | Unordered -> "</ul>\n")
    | Code_block(lang, c) ->
        if lang = "" then
          Buffer.add_string b "<pre><code>"
        else
          Printf.bprintf b "<pre class='%s'><code class='%s'>" lang lang;
        Buffer.add_string b c;
        Buffer.add_string b "</code></pre>"
    | Thematic_break ->
        Buffer.add_string b "<hr />\n"
    | Html_block body ->
        Buffer.add_string b body
    | Atx_heading (i, md) ->
        let md = f md in
        let id = "foo" in
        Buffer.add_string b (Printf.sprintf "<h%d id=\"" i);
        Buffer.add_string b id;
        Buffer.add_string b "\">";
        Buffer.add_string b md;
        Buffer.add_string b (Printf.sprintf "</h%d>" i)
  in
  loop ~p:true 0 md;
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

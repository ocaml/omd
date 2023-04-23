open Cst.Impl

let pf = Format.fprintf
let pp_list = Format.pp_print_list

let escape_link_destination s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ('(' | ')') as c ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | _ as c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let has_backticks s =
  let b = ref false in
  let len = String.length s in
  String.iteri
    (fun i -> function
      | '`' -> if i + 2 < len && String.sub s i 3 = "```" then b := true
      | _ -> ())
    s;
  !b

let rec inline ppf = function
  (* Don't introduce a thematic break *)
  | Text (_, s) when s = "***" || s = "___" || s = "---" -> pf ppf "    %s" s
  | Text (_, s) -> pf ppf "%s" s
  | Emph (_attrs, emph_style, il) ->
      let emp_style = match emph_style with Star -> "*" | Underscore -> "_" in
      pf ppf "%s%a%s" emp_style inline il emp_style
  | Strong (_attrs, emph_style, il) ->
      let emp_style = match emph_style with Star -> "*" | Underscore -> "_" in
      pf ppf "%s%s%a%s%s" emp_style emp_style inline il emp_style emp_style
  | Code (attrs, s) -> pf ppf "`%s`%a" s attributes attrs
  | Hard_break _ -> pf ppf "  @ "
  | Soft_break _ -> pf ppf "@ "
  | Link (attrs, { label; destination; title = None }) ->
      pf
        ppf
        "[%a](%s)%a"
        inline
        label
        (escape_link_destination destination)
        attributes
        attrs
  | Link (attrs, { label; destination; title = Some title }) ->
      pf
        ppf
        "[%a](%s \"%s\")%a"
        inline
        label
        (escape_link_destination destination)
        title
        attributes
        attrs
  | Image (attrs, { label; destination; title = None }) ->
      pf
        ppf
        "![%a](%s)%a"
        inline
        label
        (escape_link_destination destination)
        attributes
        attrs
  | Image (attrs, { label; destination; title = Some title }) ->
      pf
        ppf
        "![%a](%s \"%s\")%a"
        inline
        label
        (escape_link_destination destination)
        title
        attributes
        attrs
  | Html (_, html) -> pf ppf "%s" html
  | Concat (_, ils) -> pf ppf "%a" (pp_list ~pp_sep:(fun _ _ -> ()) inline) ils

and block ?(tight = false) ?(list = None) ppf = function
  | Thematic_break _ -> (
      match list with
      | Some '-' -> pf ppf "***\n"
      | Some _ | None -> pf ppf "---\n")
  | Paragraph (_, il) ->
      if tight then pf ppf "%a" inline il else pf ppf "%a@ " inline il
  | List (_, typ, spacing, blockss) ->
      let tight = spacing = Tight in
      let elt typ ppf =
        match typ with
        | Bullet c ->
            pf ppf "%c @[<v0>%a@]" c (pp_list (block ~tight ~list:(Some c)))
        | Ordered (i, c) ->
            pf ppf "%i%c @[<v0>%a@]" i c (pp_list (block ~tight ~list:(Some c)))
      in
      pf ppf "@[<v0>%a@]" (pp_list (elt typ)) blockss
  | Heading (attrs, heading_type, size, il) -> (
      match heading_type with
      | Latx ->
          pf ppf "%s %a%a" (String.make size '#') inline il attributes attrs
      | Lsetext len ->
          pf
            ppf
            "%a%a@ %s"
            inline
            il
            attributes
            attrs
            (String.make len (if size = 1 then '=' else '-')))
  | Code_block (attrs, lang, code) -> (
      let len = String.length code in
      let code = if len > 0 then String.sub code 0 (len - 1) else code in
      let cb = if has_backticks code then "~~~" else "```" in
      match (code, lang) with
      | "", "" -> pf ppf "%s%a%s" cb attributes attrs cb
      | "", lang -> pf ppf "%s%s@ %a%s" cb lang attributes attrs cb
      | code, _ -> pf ppf "%s%s %a@ %s@ %s" cb lang attributes attrs code cb)
  | Html_block (_, s) -> pf ppf "%s" s
  | Blockquote (_, blocks) -> pf ppf "> %a" (pp_list block) blocks
  | Definition_list _ -> assert false
  | Table (_, _, _) -> assert false

and attributes ppf attrs =
  if List.length attrs = 0 then ()
  else
    let attr ppf = function
      | _, "" -> ()
      | "class", s -> pf ppf ".%s" s
      | "id", s -> pf ppf "#%s" s
      | k, v -> pf ppf "%s=%s" k v
    in
    let split_attrs =
      List.(
        fold_left
          (fun acc (k, v) ->
            rev (map (fun v' -> (k, v')) (String.split_on_char ' ' v)) @ acc)
          []
          attrs)
      |> List.rev
    in
    pf ppf "{ %a }" (pp_list ~pp_sep:(fun ppf _ -> pf ppf " ") attr) split_attrs

let pp ppf = pf ppf "@[<v0>%a@]" (pp_list block)

type list_kind =
  | Ordered of int * char
  | Unordered of char

let same_list_kind k1 k2 =
  match k1, k2 with
  | Ordered (_, c1), Ordered (_, c2)
  | Unordered c1, Unordered c2 -> c1 = c2
  | _ -> false

type list_style =
  | Loose
  | Tight

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type 'a block =
  | Paragraph of 'a
  | List of list_kind * list_style * 'a block list list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of string * string option
  | Html_block of string
  | Link_def of 'a link_def

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List (k, st, xs) -> List (k, st, List.map (List.map (map f)) xs)
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading (i, x) -> Heading (i, f x)
  | Code_block _ as x -> x
  | Html_block _ as x -> x
  | Link_def {label; destination; title} -> Link_def {label = f label; destination; title}

let extract_defs ast =
  let rec loop acc = function
    | List (_, _, l) -> List.fold_left (List.fold_left loop) acc l
    | Blockquote l -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

type inline =
  | Concat of inline list
  | Text of string
  | Emph of inline
  | Bold of inline
  | Code of string
  | Hard_break
  | Soft_break
  | Url of inline * string * string option
  (* | Ref of string * string *)
  (* | Img_ref of string * string *)
  | Html of string
  | Img of string * string * string

let concat = function
  | [] -> Text ""
  | [x] -> x
  | l -> Concat l

let rec normalize_label = function
  | Concat l -> String.concat "" (List.map normalize_label l)
  | Text s -> s
  | _ -> ""
  (* | _ -> "" *)
  (* | Emph x -> "*" ^ normalize_label x ^ "*" *)
  (* | Bold x -> "**" ^ normalize_label x ^ "**" *)
  (* | Code s -> "`" ^ s ^ "`" *)

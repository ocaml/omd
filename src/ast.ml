module Attributes =
struct
  type t =
    {
      id: string option;
      classes: string list;
      attributes: (string * string) list;
    }

  let empty = {id=None; classes=[]; attributes=[]}
end

module Link_def =
struct
  type 'a t =
    {
      label: 'a;
      destination: string;
      title: string option;
      attributes: Attributes.t;
    }
end

module Block_list = struct
  type kind =
    | Ordered of int * char
    | Unordered of char

  type style =
    | Loose
    | Tight

  type 'block t =
  {
    kind: kind;
    style: style;
    blocks: 'block list list;
  }
end

module Code_block = struct
  type kind =
    | Tilde
    | Backtick

  type t =
    {
      kind: kind option;
      label: string option;
      other: string option;
      code: string option;
      attributes: Attributes.t;
    }
end

module Heading = struct
  type 'block t =
    {
      level: int;
      text: 'block;
      attributes: Attributes.t;
    }
end

module Def_list = struct
  type 'a elt = { term : 'a; defs : 'a list }
  type 'a t =
  {
    content: 'a elt list
  }
end

module Tag_block = struct
  type 'block t =
  {
    tag: string;
    content: 'block list;
    attributes: Attributes.t
  }
end

type 'a block =
  | Paragraph of 'a
  | List of 'a block Block_list.t
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of 'a Heading.t
  | Code_block of Code_block.t
  | Html_block of string
  | Link_def of string Link_def.t
  | Def_list of 'a Def_list.t
  | Tag_block of 'a block Tag_block.t

module Emph = struct
  type kind =
    | Normal
    | Strong

  type style =
    | Star
    | Underscore

  type 'inline t =
  {
    style: style;
    kind: kind;
    content: 'inline;
  }
end

module Code = struct
  type t =
  {
    level: int;
    content: string;
    attributes: Attributes.t;
  }
end

type link_kind =
  | Img
  | Url

module Link = struct
  type kind = link_kind

  type 'inline t =
  {
    kind: kind;
    def: 'inline Link_def.t;
  }
end

module Ref = struct
  type kind = link_kind

  type 'inline t =
  {
    kind: kind;
    label: 'inline;
    def: string Link_def.t;
  }
end

module Tag = struct
  type 'inline t =
  {
    tag: string;
    content: 'inline;
    attributes: Attributes.t
  }
end

type inline =
  | Concat of inline list
  | Text of string
  | Emph of inline Emph.t
  | Code of Code.t
  | Hard_break
  | Soft_break
  | Link of inline Link.t
  | Ref of inline Ref.t
  | Html of string
  | Tag of inline Tag.t

let rec map f = function
  | Paragraph x -> Paragraph (f x)
  | List l -> List  {l with blocks = List.map (List.map (map f)) l.blocks}
  | Blockquote xs -> Blockquote (List.map (map f) xs)
  | Thematic_break -> Thematic_break
  | Heading h -> Heading {h with text = f h.text}
  | Def_list l -> Def_list {content = List.map (fun elt -> {Def_list.term = f elt.Def_list.term; defs = List.map f elt.defs}) l.content}
  | Tag_block t -> Tag_block {t with content = List.map (map f) t.content}
  | Code_block _ | Html_block _ | Link_def _ as x -> x

let defs ast =
  let rec loop acc = function
    | List l -> List.fold_left (List.fold_left loop) acc l.blocks
    | Blockquote l | Tag_block {content = l; _} -> List.fold_left loop acc l
    | Paragraph _ | Thematic_break | Heading _
    | Def_list _ | Code_block _ | Html_block _ -> acc
    | Link_def def -> def :: acc
  in
  List.rev (List.fold_left loop [] ast)

module Mapper =
struct
  type t =
    { document:       t -> inline block list         -> inline block list
    ; blocks:         t -> inline block list         -> inline block list
    ; attributes:     t -> Attributes.t              -> Attributes.t
    ; block:          t -> inline block              -> inline block
    ; paragraph:      t -> inline                    -> inline
    ; blockquote:     t -> inline block list         -> inline block list
    ; list:           t -> inline block Block_list.t -> inline block Block_list.t
    ; list_item:      t -> inline block list         -> inline block list
    ; code_block:     t -> Code_block.t              -> Code_block.t
    ; thematic_break: t -> unit                      -> unit
    ; html_block:     t -> string                    -> string
    ; heading:        t -> inline Heading.t          -> inline Heading.t
    ; def_list:       t -> inline Def_list.t         -> inline Def_list.t
    ; tag_block:      t -> inline block Tag_block.t  -> inline block Tag_block.t
    ; inline:         t -> inline                    -> inline
    ; concat:         t -> inline list               -> inline list
    ; text:           t -> string                    -> string
    ; emph:           t -> inline Emph.t             -> inline Emph.t
    ; code:           t -> Code.t                    -> Code.t
    ; hard_break:     t -> unit                      -> unit
    ; soft_break:     t -> unit                      -> unit
    ; html:           t -> string                    -> string
    ; link:           t -> inline Link.t             -> inline Link.t
    ; ref:            t -> inline Ref.t              -> inline Ref.t
    ; tag:            t -> inline Tag.t              -> inline Tag.t
    }

  let default : t =
    let identity = (fun _m x -> x) in
    let link_def (r :t) (d :_ Link_def.t) =
      { d with attributes = r.attributes r d.attributes }
    in
    {
      document = (fun r l -> r.blocks r l);
      blocks = (fun r l -> List.map (r.block r) l);
      list_item = (fun r li -> r.blocks r li);
      attributes = identity;
      block = begin fun r -> function
        | Paragraph i -> Paragraph (r.paragraph r i)
        | List list -> List (r.list r list)
        | Blockquote blocks -> Blockquote (r.blockquote r blocks)
        | Thematic_break -> r.thematic_break r (); Thematic_break
        | Heading h -> Heading (r.heading r h)
        | Code_block c -> Code_block (r.code_block r c)
        | Html_block h -> Html_block (r.html_block r h)
        | Link_def d -> Link_def (link_def r d)
        | Def_list d -> Def_list (r.def_list r d)
        | Tag_block t -> Tag_block (r.tag_block r t)
      end;
      paragraph = (fun r p -> r.inline r p);
      blockquote = (fun r blocks -> r.blocks r blocks);
      list = begin fun r l ->
        { l with blocks = List.map (r.blocks r) l.blocks }
      end;
      code_block = (fun r c -> {c with attributes = r.attributes r c.attributes});
      thematic_break = identity;
      html_block = identity;
      heading = begin fun r h ->
        { h with
          text = r.inline r h.text
        ; attributes = r.attributes r h.attributes}
      end;
      def_list = begin fun r {content} ->
        let open Def_list in
        { content =
            List.map
              begin fun { term; defs } ->
                { term = r.inline r term
                ; defs = List.map (r.inline r) defs }
              end
              content }
      end;
      tag_block = begin fun r t ->
        { t with
          attributes = r.attributes r t.attributes
        ; content = r.blocks r t.content }
      end;
      inline = begin fun r -> function
        | Concat il -> Concat (List.map (r.inline r) il)
        | Text t -> Text (r.text r t)
        | Emph e -> Emph (r.emph r e)
        | Code c -> Code (r.code r c)
        | Hard_break -> r.hard_break r (); Hard_break
        | Soft_break -> r.soft_break r (); Soft_break
        | Link l -> Link (r.link r l)
        | Ref ref -> Ref (r.ref r ref)
        | Html h -> Html (r.html r h)
        | Tag t -> Tag (r.tag r t)
      end;
      concat = (fun r il -> List.map (r.inline r) il);
      text = identity;
      emph = begin fun r e ->
        { e with content = r.inline r e.content }
      end;
      code = begin fun r c ->
        { c with attributes = r.attributes r c.attributes }
      end;
      hard_break = identity;
      soft_break = identity;
      html = identity;
      link = (fun r l -> { l with def = link_def r l.def });
      ref = begin fun r ref ->
        { ref with
          label = r.inline r ref.label
        ; def = link_def r ref.def }
      end;
      tag = begin fun r t ->
        { t with
          content = r.inline r t.content
        ; attributes = r.attributes r t.attributes }
      end;
    }
end

module Iterator =
struct
  type 'a t =
    { document:       'a t -> 'a -> inline block list         -> unit
    ; blocks:         'a t -> 'a -> inline block list         -> unit
    ; attributes:     'a t -> 'a -> Attributes.t              -> unit
    ; block:          'a t -> 'a -> inline block              -> unit
    ; paragraph:      'a t -> 'a -> inline                    -> unit
    ; blockquote:     'a t -> 'a -> inline block list         -> unit
    ; list:           'a t -> 'a -> inline block Block_list.t -> unit
    ; list_item:      'a t -> 'a -> inline block list         -> unit
    ; code_block:     'a t -> 'a -> Code_block.t              -> unit
    ; thematic_break: 'a t -> 'a -> unit                      -> unit
    ; html_block:     'a t -> 'a -> string                    -> unit
    ; heading:        'a t -> 'a -> inline Heading.t          -> unit
    ; def_list:       'a t -> 'a -> inline Def_list.t         -> unit
    ; tag_block:      'a t -> 'a -> inline block Tag_block.t  -> unit
    ; inline:         'a t -> 'a -> inline                    -> unit
    ; concat:         'a t -> 'a -> inline list               -> unit
    ; text:           'a t -> 'a -> string                    -> unit
    ; emph:           'a t -> 'a -> inline Emph.t             -> unit
    ; code:           'a t -> 'a -> Code.t                    -> unit
    ; hard_break:     'a t -> 'a -> unit                      -> unit
    ; soft_break:     'a t -> 'a -> unit                      -> unit
    ; html:           'a t -> 'a -> string                    -> unit
    ; link:           'a t -> 'a -> inline Link.t             -> unit
    ; ref:            'a t -> 'a -> inline Ref.t              -> unit
    ; tag:            'a t -> 'a -> inline Tag.t              -> unit
    }

  let default : 'a t =
    let identity = (fun _ _ _ -> ()) in
    let link_def (r :'a t) ctx (d :_ Link_def.t) =
      r.attributes r ctx d.attributes
    in
    {
      document = (fun r ctx l -> r.blocks r ctx l);
      blocks = (fun r ctx l -> List.iter (r.block r ctx) l);
      list_item = (fun r ctx li -> r.blocks r ctx li);
      attributes = identity;
      block = begin fun r ctx -> function
        | Paragraph i -> (r.paragraph r ctx i)
        | List list -> (r.list r ctx list)
        | Blockquote blocks -> (r.blockquote r ctx blocks)
        | Thematic_break -> r.thematic_break r ctx ()
        | Heading h -> (r.heading r ctx h)
        | Code_block c -> (r.code_block r ctx c)
        | Html_block h -> (r.html_block r ctx h)
        | Link_def d -> (link_def r ctx d)
        | Def_list d -> (r.def_list r ctx d)
        | Tag_block t -> (r.tag_block r ctx t)
      end;
      paragraph = (fun r ctx p -> r.inline r ctx p);
      blockquote = (fun r ctx blocks -> r.blocks r ctx blocks);
      list = begin fun r ctx l ->
        List.iter (r.blocks r ctx) l.blocks
      end;
      code_block = (fun r ctx c -> r.attributes r ctx c.attributes);
      thematic_break = identity;
      html_block = identity;
      heading = begin fun r ctx h ->
        r.inline r ctx h.text;
        r.attributes r ctx h.attributes
      end;
      def_list = begin fun r ctx {content} ->
        let open Def_list in
        List.iter
          begin fun { term; defs } ->
            r.inline r ctx term;
            List.iter (r.inline r ctx) defs;
          end
          content
      end;
      tag_block = begin fun r ctx t ->
        r.attributes r ctx t.attributes;
        r.blocks r ctx t.content
      end;
      inline = begin fun r ctx -> function
        | Concat il -> (List.iter (r.inline r ctx) il)
        | Text t -> (r.text r ctx t)
        | Emph e -> (r.emph r ctx e)
        | Code c -> (r.code r ctx c)
        | Hard_break -> r.hard_break r ctx ()
        | Soft_break -> r.soft_break r ctx ()
        | Link l -> (r.link r ctx l)
        | Ref ref -> (r.ref r ctx ref)
        | Html h -> (r.html r ctx h)
        | Tag t -> (r.tag r ctx t)
      end;
      concat = (fun r ctx il -> List.iter (r.inline r ctx) il);
      text = identity;
      emph = (fun r ctx e -> r.inline r ctx e.content);
      code = (fun r ctx c -> r.attributes r ctx c.attributes);
      hard_break = identity;
      soft_break = identity;
      html = identity;
      link = (fun r ctx l -> link_def r ctx l.def);
      ref = begin fun r ctx ref ->
        r.inline r ctx ref.label;
        link_def r ctx ref.def
      end;
      tag = begin fun r ctx t ->
        r.inline r ctx t.content;
        r.attributes r ctx t.attributes
      end;
    }
end

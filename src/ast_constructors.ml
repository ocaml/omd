open Ast.Impl

module type Intf = sig
  (** Functions to help constructing the elements of a {!doc}.

      E.g.,

      {[
        let open Omd.Ctor in
        let para =
          p ~attrs:[ ("class", "my-para") ] [ txt "Content of"; em "this"; txt "paragraph" ]
        in
        [ blockquote [ para; hr; p [ txt "Content of second paragraph" ] ] ]
      ]}

      Produces

      {v
<blockquote>
<p class="my-para">Content of<em>this</em>paragraph</p>
<hr />
<p>Content of second paragraph</p>
</blockquote>
      v}

      The optional [attrs] argument always defaults to an empty list, and can
      generally be omitted. *)

  (** {3 Constructors for inline elements}  *)

  val txt : ?attrs:attributes -> string -> attributes inline
  (** [txt ~attrs s] is {{!Text} [Text (attrs, s)]}. *)

  val em : ?attrs:attributes -> string -> attributes inline
  (** [em ~attrs s] is {{!Emph} [Emph (attrs, txt s)]}. See {!txt}. *)

  val strong : ?attrs:attributes -> string -> attributes inline
  (** [strong ~attrs s] is {{!Strong} [Strong (attrs, txt s)]}. See {!txt}. *)

  val code : ?attrs:attributes -> string -> attributes inline
  (** [code ~attrs s] is {{!Code} [Code (attrs, s)]}. *)

  val br : attributes inline
  (** [br] is {{!Hard_break}[Hard_break []]}. *)

  val nl : attributes inline
  (** [nl] is {{!Soft_break}[Soft_break []]}. *)

  val a :
       ?attrs:attributes
    -> ?title:string
    -> url:string
    -> string
    -> attributes inline
  (** [a ~attrs ~title ~url label] is a link around the text of [label],
      pointing to the [url], with the optional title [title] and additional [attrs].
      See {!Link}. *)

  val img :
       ?attrs:attributes
    -> ?title:string
    -> alt:string
    -> string
    -> attributes inline
  (** [img ~attrs ~title ~alt src] is an image from the given [src] that has the
      [alt] text as a fallback, with the optional title [title] and additional
      [attrs].  See {!Image}. *)

  val html : string -> attributes inline
  (** [html s] is an inline HTML string. See {!Html}. *)

  (** {3 Constructors for block-level elements} *)

  val p : ?attrs:attributes -> attributes inline list -> attributes block
  (** [p ~attrs inlines] is a pragraph block holding the given [inline]
      elements. See {!Paragraph}. *)

  val ul :
       ?attrs:attributes
    -> ?spacing:list_spacing
    -> attributes block list list
    -> attributes block
  (** [ul ~attrs ~spacing items] is an unordered list with the specified [spacing], listing
      the given [items]. Each item is a list of block elements.

      - [spacing] defaults to {!Loose}.

      E.g.,

      {[
        ul ~spacing:Tight
          [ [ p [ txt "Item 1" ] ]
          ; [ p [ txt "Item 2" ] ]
          ; [ p [ txt "Item 3" ] ]
          ]
      ]}

      See {!List} and {!Bullet}. *)

  val ol :
       ?attrs:attributes
    -> ?start:int
    -> ?char:[ `Dot | `Paren ]
    -> ?spacing:list_spacing
    -> attributes block list list
    -> attributes block
  (** [ol ~attrs ~start ~char ~spacing items] is like {!ul}, but constructs an ordered list,
      where [start] is the number to start enumerating from, and [char] indicates the
      character following the number in the enumeration.

      - [char] can be either [`Dot] indicating ['.'] or [`Paren] indicating [')'], and
        defaults to [`Dot].
      - [start] defaults to [1].

      See {!List} and {!Ordered}. *)

  val blockquote :
    ?attrs:attributes -> attributes block list -> attributes block
  (** [blockquote ~attrs blocks] is a blockquote element containing the given
      [blocks]. See {!Blockquote}. *)

  val hr : attributes block
  (** [hr] is {{!Thematic_break} [Thematic_break []]}. *)

  val h : ?attrs:attributes -> int -> attributes inline list -> attributes block
  (** [h ~attrs level inlines] is a heading of the given [level] comprised of
      the [inlines]. See {!Heading}. *)

  val code_bl : ?attrs:attributes -> ?lang:string -> string -> attributes block
  (** [code_bl ~attrs ~lang code] is a code block labeled with language [lang].

      - [lang] defaults to being absent.

      See {!Code_block} *)

  val html_bl : ?attrs:attributes -> string -> attributes block
  (** [html_bl ~attrs html] is a block-level element of raw HTML. See {!Html_block}. *)

  type 'attr ctor_def_elt =
    { term : 'attr inline list
    ; defs : 'attr inline list list
    }
  (** Type for the items given to {!dl} definition lists. It is isomorphic to {!def_elt}. *)

  val dl : ?attrs:attributes -> attributes ctor_def_elt list -> attributes block
  (** [dl ~attrs elements] is a definition list of the given [elements]. See
      {!Definition_list}.

      E.g.,

      {[
        dl
          [ { term = [ txt "def term 1" ]
            ; defs =
                [ [ txt "definition 1.1" ]
                ; [ txt "definition 1.2" ]
                ; [ txt "definition 1.3" ]
                ]
            }
          ; { term = [ txt "def term 2" ]
            ; defs =
                [ [ txt "definition 2.1" ]
                ; [ txt "definition 2.2" ]
                ; [ txt "definition 2.3" ]
                ]
            }
          ]
      ]} *)
end

module Impl : Intf = struct
  let concat elems = Concat ([], elems)
  let txt ?(attrs = []) s = Text (attrs, s)
  let em ?(attrs = []) s = Emph (attrs, txt s)
  let strong ?(attrs = []) s = Strong (attrs, txt s)
  let code ?(attrs = []) s = Code (attrs, s)
  let br = Hard_break []
  let nl = Soft_break []

  let a ?(attrs = []) ?title ~url label =
    Link (attrs, { label = txt label; destination = url; title })

  let img ?(attrs = []) ?title ~alt src =
    Image (attrs, { label = txt alt; destination = src; title })

  (* Note that attributes are not actually supported Html nodes currently. *)
  let html s = Html ([], s)

  (* Block constructors *)

  let p ?(attrs = []) inlines = Paragraph (attrs, concat inlines)

  let ul ?(attrs = []) ?(spacing = Loose) items =
    List (attrs, Bullet '-', spacing, items)

  let ol ?(attrs = []) ?(start = 1) ?(char = `Dot) ?(spacing = Loose) items =
    let c = match char with `Dot -> '.' | `Paren -> ')' in
    List (attrs, Ordered (start, c), spacing, items)

  let blockquote ?(attrs = []) blocks = Blockquote (attrs, blocks)
  let hr = Thematic_break []
  let h ?(attrs = []) level inlines = Heading (attrs, level, concat inlines)
  let code_bl ?(attrs = []) ?(lang = "") s = Code_block (attrs, lang, s)
  let html_bl ?(attrs = []) s = Html_block (attrs, s)

  type 'attr ctor_def_elt =
    { term : 'attr inline list
    ; defs : 'attr inline list list
    }

  let dl ?(attrs = []) (items : 'attr ctor_def_elt list) =
    let def_elt_of_pair { term; defs } : 'attr def_elt =
      let term = concat term in
      let defs = List.map concat defs in
      { term; defs }
    in
    let def_elts = List.map def_elt_of_pair items in
    Definition_list (attrs, def_elts)
end

(** A markdown parser in OCaml. *)

type list_kind =
  | Ordered of int * char
  | Unordered of char

type list_style =
  | Loose
  | Tight

type 'a link_def =
  {
    label: 'a;
    destination: string;
    title: string option;
  }

type fenced_code_kind =
  | Tilde
  | Backtick

type 'a block =
  | Paragraph of 'a
  | List of list_kind * list_style * 'a block list list
  | Blockquote of 'a block list
  | Thematic_break
  | Heading of int * 'a
  | Code_block of (fenced_code_kind * string) option * string option
  | Html_block of string
  | Link_def of string link_def

type emph_kind =
  | Normal
  | Strong

type emph_style =
  | Star
  | Underscore

type link_kind =
  | Img
  | Url

type inline =
  | Concat of inline list
  | Text of string
  | Emph of emph_kind * emph_style * inline
  | Code of string
  | Hard_break
  | Soft_break
  | Link of link_kind * inline link_def
  | Ref of link_kind * inline * string link_def
  | Html of string

type t = inline block list
(** A markdown document *)

val of_channel: in_channel -> t

val of_string: string -> t

val to_html: (* ?pindent:bool -> ?nl2br:bool -> *) t -> string
(** Translate markdown representation into raw HTML. *)

val to_sexp: t -> string

(* val to_markdown: t -> string *)
(** Translate markdown representation into textual markdown. *)

(* val to_text: t -> string *)
(** Translate markdown representation into raw text. *)

(* val toc: ?start:int list -> ?depth:int -> t -> t *)
(** [toc md] returns [toc] a table of contents for [md].

    @param start gives the section for which the TOC must be built.
    For example [~start:[2;3]] will build the TOC for subsections of
    the second [H1] header, and within that section, the third [h2]
    header.  If a number is [0], it means to look for the first
    section at that level but stop if one encounters any other
    subsection.  If no subsection exists, an empty TOC [[]] will be
    returned.  Default: [[]] i.e. list all sections, starting with the
    first [H1].

    @param depth the table of contents.  Default: [2].  *)

type t =
  | Cat of t list
  | Text of string
  | Emph of t
  | Bold of t
  | Code of string
  | Br
  | NL
  | Url of href * t * title
  | Ref of name * string * fallback
  | Img_ref of name * alt * fallback
  | Html of string
  | Raw of string
  | Img of alt * src * title

and fallback = < to_string : string ; to_t : t >
and name = string
and alt = string
and src = string
and href = string
and title = string

val print : Format.formatter -> t -> unit

val html_of_md : t -> string
val markdown_of_md : t -> string

module type Env =
sig
  val default_lang : string
  (** default language for code blocks *)

  val gh_uemph_or_bold_style : bool
  (** flag: bold/emph using using underscores is by default
      github-style, which means that underscores inside words are
      left as underscore, rather than special characters, because
      it's more convenient. However it is also less expressive
      because then you can't bold/emph a part of a word. You might
      want to set this flag to false. *)

  val blind_html : bool
  (** flag: if true, will not check whether a used HTML tag actually
      exists in HTML. *)

  val strict_html : bool
  (** flag: if true, will only accept known inline HTML tags in inline HTML. *)

  val warning : bool
  (** flag: if true, will output warnings *)

  val warn_error : bool
  (** flag: if true, will convert warnings to errors *)
end

module Default_env : functor (Unit: sig end) -> Env

module Make : functor (Env : Env) -> sig
  val default_lang : string
  (** default language for code blocks *)

  val gh_uemph_or_bold_style : bool
  (** flag: bold/emph using using underscores is by default
      github-style, which means that underscores inside words are
      left as underscore, rather than special characters, because
      it's more convenient. However it is also less expressive
      because then you can't bold/emph a part of a word. You might
      want to set this flag to false. *)

  val blind_html : bool
  (** flag: if true, will not check whether a used HTML tag actually
      exists in HTML. *)

  val strict_html : bool
  (** flag: if true, will only accept known inline HTML tags in inline HTML. *)

  val htmlcodes_set : Utils.StringSet.t
  (** set of known HTML codes *)

  val inline_htmltags_set : Utils.StringSet.t
  (** set of known inline HTML tags *)

  val htmltags_set : Utils.StringSet.t
  (** All known HTML tags *)

  val parse : Representation.tok list -> t
end

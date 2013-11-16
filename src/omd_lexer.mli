val string_of_t : Omd_representation.tok -> string

val lex : string -> Omd_representation.tok list
(** Translate a raw string into tokens for the parser.  To implement
    an extension to the lexer, one may process its result before
    giving it to the parser. To implement an extension to the
    parser, one may extend it using the constructor [Tag]
    from type [tok] and/or using the extensions mechanism
    of the parser (cf. the optional argument [extensions]).
    The main difference is that [Tag] is processed by the parser
    in highest priority whereas functions in [extensions] are applied
    with lowest priority. *)

val size : Omd_representation.tok -> int * int
val make_space : int -> Omd_representation.tok
val position :
  'a ->
  Omd_representation.tok list -> Omd_representation.tok list -> int * int
val string_of_tl : Omd_representation.tok list -> string
val destring_of_tl : ?limit:int -> Omd_representation.tok list -> string

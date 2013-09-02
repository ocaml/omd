val string_of_t : Omd_representation.tok -> string
val lex : string -> Omd_representation.tok list
val size : Omd_representation.tok -> int * int
val make_space : int -> Omd_representation.tok
val position :
  'a ->
  Omd_representation.tok list -> Omd_representation.tok list -> int * int
val string_of_tl : Omd_representation.tok list -> string
val destring_of_tl : Omd_representation.tok list -> string

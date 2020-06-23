type desc =
  | Spaces
  | UnicodeSpace
  | LineEnd
  | WordChars
  | Symbol of char

type pos =
  {
    filename: string;
    line: int;
    column: int;
  }

type token =
  {
    desc: desc;
    pos: pos;
    contents: string;
  }

val parse_file: string -> token list

val parse_string: filename:string -> string -> token list

val unparse: token list -> string

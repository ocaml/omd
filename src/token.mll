{
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

let add_cols n pos =
  {pos with column = pos.column + n}

let succ_line pos =
  {pos with line = pos.line + 1; column = 1}

let start_pos filename =
  {filename; line = 1; column = 1}

let mk desc pos lexbuf =
  {desc; pos; contents = Lexing.lexeme lexbuf}

let lexeme_length lexbuf =
  Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf + 1
}

rule go accu pos = parse
  | ' '+                     { go (mk Spaces pos lexbuf :: accu)
                                 (add_cols (lexeme_length lexbuf) pos) lexbuf }
  | ['\r''\n']+              { go (mk LineEnd pos lexbuf :: accu) (succ_line pos) lexbuf }
  | ['a'-'z''A'-'Z''0'-'9']+ { go (mk WordChars pos lexbuf :: accu)
                                 (add_cols (lexeme_length lexbuf) pos) lexbuf }
  | '\t'                     { go (mk Spaces pos lexbuf :: accu)
                                 (add_cols (4 - (pos.column - 1) mod 4) pos) lexbuf }
  | _ as c                   { go (mk (Symbol c) pos lexbuf :: accu) (add_cols 1 pos) lexbuf }
  | eof                      { List.rev accu }

{
let with_open_in fn f =
  let ic = open_in fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> f ic)

let parse_file filename =
  with_open_in filename @@ fun ic ->
  go [] (start_pos filename)
    (Lexing.from_channel ~with_positions:false ic)

let parse_string ~filename s =
  go [] (start_pos filename)
    (Lexing.from_string ~with_positions:false s)

let unparse toks =
  String.concat "" (List.map (fun {contents; _} -> contents) toks)
}

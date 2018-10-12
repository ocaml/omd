(***********************************************************************)
(* omd: Markdown frontend in OCaml                                     *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(* Implementation notes *********************************************

   * - This module should depend on OCaml's standard library only and
   * should be as 'pure OCaml' (i.e. depend as least as possible on
   * external tools) as possible.

   * - `while' loops are sometimes preferred to recursion because this
   * may be used on systems where tail recursion is not well
   * supported. (I tried to write "while" as often as possible, but it
   * turned out that it was pretty inconvenient, so I do use
   * recursion.  When I have time, I'll do some tests and see if I
   * need to convert recursive loops into iterative loops. Sorry if it
   * makes it harder to read.)

*)

(* class type tag = object method is_me : 'a. 'a -> bool end *)

open Representation

type token = Representation.tok
type t = Representation.tok list

let char_of_delim = function
  | Ampersand -> '&'
  | At -> '@'
  | Backquote -> '`'
  | Backslash -> '\\'
  | Bar -> '|'
  | Caret -> '^'
  | Cbrace -> '}'
  | Colon -> ':'
  | Comma -> ','
  | Cparenthesis -> ')'
  | Cbracket -> ']'
  | Dollar -> '$'
  | Dot -> '.'
  | Doublequote -> '\"'
  | Exclamation -> '!'
  | Equal -> '='
  | Greaterthan -> '>'
  | Hash -> '#'
  | Lessthan -> '<'
  | Minus -> '-'
  | Newline -> '\n'
  | Obrace -> '{'
  | Oparenthesis -> '('
  | Obracket -> '['
  | Percent -> '%'
  | Plus -> '+'
  | Question -> '?'
  | Quote -> '\''
  | Semicolon -> ';'
  | Slash -> '/'
  | Space -> ' '
  | Star -> '*'
  | Tab -> assert false
  | Tilde -> '~'
  | Underscore -> '_'

let string_of_token = function
  | Tag (name, o) -> if Utils.debug then "TAG("^name^")" ^ o#to_string else o#to_string
  | Word s | Number s -> s
  | Delim (n, Tab) -> assert (n >= 0); String.make (4*n) ' '
  | Delim (n, d) -> assert (n >= 0); String.make n (char_of_delim d)

let size_and_newlines = function
  | Tag _ -> 0, 0
  | Delim (x, Newline) -> 0, x
  | Delim (x, _) -> x, 0
  | Number s | Word s -> String.length s, 0

let length t =
  let c, nl = size_and_newlines t in
  c + nl

let split_first = function
  | Delim (1, _) | Number _ | Tag _ | Word _ ->
      invalid_arg "Omd_lexer.split_first"
  | Delim (n, d) ->
      assert (n >= 2);
      Delim (1, d), Delim (n-1, d)

module type Input =
sig
  type t
  val length : t -> int
  val get : t -> int -> char
  val sub : t -> pos:int -> len:int -> string
end

module Lex(I : Input) :
sig
  val lex : I.t -> t
end =
struct
  let lex (s : I.t) =
    let result = ref [] in
    let i = ref 0 in
    let l = I.length s in
    let rcount c =
      (* [rcount c] returns the number of immediate consecutive
         occurrences of [c].  By side-effect, it increases the reference
         counter [i]. *)
      let rec loop r =
        if !i = l then r
        else if I.get s !i = c then (incr i; loop (r+1))
        else r
      in
      loop 1
    in
    let word () =
      let start = !i in
      let rec loop () =
        begin
          if !i = l then
            Word(I.sub s ~pos:start ~len:(!i-start))
          else
            match I.get s !i with
            | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\''
            | '"' | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':'
            | ';' | '>' | '~' | '<' | '@' | '&' | '|' | '^' | '.' | '/'
            | '$' | '%' | '!' | '?' | '=' ->
                Word(I.sub s ~pos:start ~len:(!i-start))
            | _ ->
                incr i;
                loop()
        end
      in
      loop()
    in
    let maybe_number () =
      let start = !i in
      while
        !i < l &&
        match I.get s !i with
        | '0' .. '9' -> true
        | _ -> false
      do
        incr i
      done;
      if !i = l then
        Number(I.sub s ~pos:start ~len:(!i-start))
      else
        begin match I.get s !i with
        | ' ' | '\t' | '\n' | '\r' | '#' | '*' | '-' | '+' | '`' | '\'' | '"'
        | '\\' | '_' | '[' | ']' | '{' | '}' | '(' | ')' | ':' | ';' | '>'
        | '~' | '<' | '@' | '&' | '|' | '^' | '.' | '/' | '$' | '%' | '!'
        | '?' | '=' ->
            Number(I.sub s ~pos:start ~len:(!i-start))
        | _ ->
            i := start;
            word()
        end
    in

    let n_occ c = incr i; rcount c in

    while !i < l do
      let c = I.get s !i in
      let w =
        let delim d = Delim (n_occ c, d) in
        match c with
        | ' '  -> delim Space
        | '\t' -> Delim (4*(n_occ c), Space) (* let n = n_occ c in if n = 1 then Spaces(2) else Spaces(4*n-2) *)
        | '\n' -> delim Newline
        | '\r' -> (* eliminating \r by converting all styles to unix style *)
            incr i;
            let rec count_rn x =
              if !i < l && I.get s (!i) = '\n' then
                if !i + 1 < l && I.get s (!i+1) = '\r' then
                  (i := !i + 2; count_rn (x+1))
                else
                  x
              else
                x
            in
            let rn = 1 + count_rn 0 in
            if rn = 1 then
              delim Newline
            else begin
              assert (rn >= 2);
              Delim (rn, Newline)
            end
        | '#'  -> delim Hash
        | '*'  -> delim Star
        | '-'  -> delim Minus
        | '+'  -> delim Plus
        | '`'  -> delim Backquote
        | '\'' -> delim Quote
        | '"'  -> delim Doublequote
        | '\\' -> delim Backslash
        | '_'  -> delim Underscore
        | '['  -> delim Obracket
        | ']'  -> delim Cbracket
        | '{'  -> delim Obrace
        | '}'  -> delim Cbrace
        | '('  -> delim Oparenthesis
        | ')'  -> delim Cparenthesis
        | ':'  -> delim Colon
        | ';'  -> delim Semicolon
        | '>'  -> delim Greaterthan
        | '~'  -> delim Tilde
        | '<'  -> delim Lessthan
        | '@'  -> delim At
        | '&'  -> delim Ampersand
        | '|'  -> delim Bar
        | '^'  -> delim Caret
        | ','  -> delim Comma
        | '.'  -> delim Dot
        | '/'  -> delim Slash
        | '$'  -> delim Dollar
        | '%'  -> delim Percent
        | '='  -> delim Equal
        | '!'  -> delim Exclamation
        | '?'  -> delim Question
        | '0' .. '9' -> maybe_number ()
        | _ -> word ()
      in
      result := w :: !result
    done;
    List.rev !result
end

module Lex_string = Lex(StringLabels)

let lex = Lex_string.lex

type bigstring = (char,
                  Bigarray.int8_unsigned_elt,
                  Bigarray.c_layout) Bigarray.Array1.t

module Bigarray_input : Input with type t = bigstring =
struct
  module BA = Bigarray

  type t = bigstring
  let get = BA.Array1.get
  let length = BA.Array1.dim
  let sub arr ~pos ~len =
    if len < 0 || pos < 0 || pos + len > BA.Array1.dim arr
    then invalid_arg "Bigarray_input.sub";
    let s = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.unsafe_set s i (BA.Array1.unsafe_get arr (i + pos))
    done;
    Bytes.to_string s
end

module Lex_bigarray = Lex(Bigarray_input)
let lex_bigarray = Lex_bigarray.lex

let make_space n =
  if n <= 0 then invalid_arg "Omd_lexer.make_space";
  Delim (n, Space)

(*
(** [string_of_tl l] returns the string representation of l.
    [estring_of_tl l] returns the escaped string representation of l
    (same semantics as [String.escaped (string_of_tl l)]). *)
let string_of_tl, estring_of_tl =
  let g escaped tl =
    let b = Buffer.create 42 in
    let rec loop : 'a t list -> unit = function
      | e::tl ->
          Buffer.add_string b (if escaped then String.escaped (string_of_t e)
                               else string_of_t e);
          loop tl
      | [] ->
          ()
    in
      Buffer.contents (loop tl; b)
  in g false, g true
*)

let string_of_tokens tl =
  let b = Buffer.create 128 in
  List.iter (fun e -> Buffer.add_string b (string_of_token e)) tl;
  Buffer.contents b


let destring_of_tokens ?(limit=max_int) tl =
  let b = Buffer.create 1024 in
  let rec loop (i:int) (tlist:tok list) : unit = match tlist with
    | e::tl ->
        if limit = i then
          loop i []
        else
          begin
            Buffer.add_string b (String.escaped (string_of_token e));
            Buffer.add_string b "::";
            loop (succ i) tl
          end
    | [] ->
        Buffer.add_string b "[]"
  in
  Buffer.contents (loop 0 tl; b)

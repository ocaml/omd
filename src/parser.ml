open Ast
open Compat

module T : sig
  type 'a t

  val run : 'a t -> string -> 'a option

  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val choose : 'a t list -> 'a t
  val left : 'a t -> unit t -> 'a t
  val right : unit t -> 'a t -> 'a t
  val when_ : unit t -> unit t
  val unless : unit t -> unit t
  val list : 'a t -> 'a list t
  val option : 'a t -> 'a option t
  val ignore : 'a t -> unit t
  val maybe : unit t -> unit t
  val chars : string -> char t
  val capture : unit t -> string t
end = struct
  type 'a t = string -> int ref -> 'a

  exception Fail

  let run t s =
    match t s (ref 0) with
    | exception Fail -> None
    | x -> Some x

  let return x _ _ = x
  let map f t s pos = f (t s pos)
  let bind t f s pos = f (t s pos) s pos
  let pair t u s pos = let x = t s pos in let y = u s pos in (x, y)
  let choose ts s pos =
    let p = !pos in
    let rec loop = function
      | [] -> raise Fail
      | t :: ts ->
          begin try t s pos with Fail -> pos := p; loop ts end
    in
    loop ts
  let left t u s pos =
    let x = t s pos in
    u s pos;
    x
  let right t u s pos =
    t s pos;
    u s pos
  let when_ t s pos =
    let p = !pos in
    match t s pos with
    | exception (Fail as e) -> pos := p; raise e
    | () -> pos := p
  let unless t s pos =
    let p = !pos in
    match t s pos with
    | exception Fail -> pos := p
    | () -> pos := p; raise Fail
  let list t s pos =
    let rec loop accu =
      let p = !pos in
      match t s pos with
      | exception Fail -> pos := p; List.rev accu
      | x -> loop (x :: accu)
    in
    loop []
  let option t s pos =
    let p = !pos in
    match t s pos with
    | exception Fail -> pos := p; None
    | x -> Some x
  let ignore t s pos =
    ignore (t s pos)
  let maybe t s pos =
    let p = !pos in
    try t s pos with Fail -> pos := p
  let chars cs s pos =
    if String.length s <= !pos then raise Fail;
    let x = s.[!pos] in
    let rec loop i =
      if i >= String.length cs then raise Fail
      else begin
        let c = cs.[i] in
        if x = c then c else loop (succ i)
      end
    in
    loop 0
  let capture t s pos =
    let p = !pos in
    let _ = t s pos in
    String.sub s p (!pos - p)
end

open T

let il il_desc = {il_desc; il_attributes = []}

let bl bl_desc = {bl_desc; bl_attributes = []}

let nonempty_list t =
  map (fun (x, xs) -> x :: xs) (pair t (list t))

let spacechar = chars " \t"

let space = map (fun _ -> il (Text " ")) (nonempty_list spacechar)

let doc = map (fun il -> [bl (Paragraph il)]) space

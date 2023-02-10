module SMap = Map.Make (String)

type t = int SMap.t

let empty = SMap.empty
let count s t = match SMap.find_opt s t with None -> 0 | Some x -> x
let incr s t = SMap.add s (count s t + 1) t

let touch s t =
  let count = count s t in
  (count, incr s t)

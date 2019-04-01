open Printf
module B = BatList
module Int = struct type t = int let compare = compare end
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

exception Parsing_failed of string
exception Runtime_error of string

let n_cartesian_product = B.n_cartesian_product

let id x = x

let flip f x y = f y x

let option d f = function
  | None -> d
  | Some x -> f x

let from_some = function
  | None -> failwith "INTERNAL: expected Some"
  | Some x -> x
                         
(* returns [i,...,k] *)
let range i k =
  let rec loop xs k = if k < i then xs else loop (k :: xs) (k - 1) in
  loop [] k

let rec repeat n x =
  if n == 0 then [] else x :: repeat (n-1) x


let map_join c f ts =
  String.concat c (List.map f ts)


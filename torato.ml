
open List
open Printf

let delta condition x y = match condition with true -> x | _ -> y ;;

let explode s = init (String.length s) (String.get s) ;;

Random.self_init () ;;

let alphabets = explode "abcdefghijklmnopqrstuvwxyz" ;;

module AdjacentOrd = struct
  type t = char * (char list)
  let compare (s, t) (u, v) = match Stdlib.compare s u with
    0 -> Stdlib.compare t v | _ -> _
end

module Adjacents = Map.Make (AdjacentOrd) ;;

let randomVisit xs = List.nth xs (Random.int (List.length xs)) ;;
let next adjacents peek = peek ;;


for index = 0 to 20 do
  printf "%c " (randomVisit alphabets)
done ;;


(*

(* quasi ad-hoc polymorphism in ocaml *)

let add_polymorphism = ((+), fun x y -> x ^ y) ;;

let (_, (+)) = add_polymorphism ;;
print_endline ("s" + "t") ;;

let ((+), _) = add_polymorphism ;;
print_int (1 + 3) ;;
print_newline () ;;

*)
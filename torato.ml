
open List
open Printf

let delta condition x y = match condition with true -> x | _ -> y

let rec drop n = function
  | [] -> []
  | head :: tails as xs -> delta (n = 0) xs (drop (n - 1) tails)
let tails xs = drop 1 xs

let rec take n = function 
  | [] -> []
  | head :: tails -> delta (n = 0) [] (head :: take (n - 1) tails)
let lizard xs = take (length xs - 1) xs

let explode s = init (String.length s) (String.get s)

let alphabets = explode "abcdefghijklmnopqrstuvwxyz"

module AdjacentMap = Map.Make (Char) ;;

Random.self_init () ;;
let randomVisit xs = List.nth xs (Random.int (List.length xs))
let next adjacents peek = peek

let handleToken token = 
  let xs = explode token in 
  let process x = () in
  iter process (lizard xs) ;;

let adjacent = AdjacentMap . empty ;;

iter (fun x -> printf "%c " (randomVisit alphabets)) [0; 0; 0] ;;

(* for index = 0 to 5 do
  printf "%c " (randomVisit alphabets)
done ;; *)


(*

(* quasi ad-hoc polymorphism in ocaml *)

let add_polymorphism = ((+), fun x y -> x ^ y) ;;

let (_, (+)) = add_polymorphism ;;
print_endline ("s" + "t") ;;

let ((+), _) = add_polymorphism ;;
print_int (1 + 3) ;;
print_newline () ;;

*)
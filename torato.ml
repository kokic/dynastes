
(* basic utils *)

open List

let delta condition x y = match condition with true -> x | _ -> y

let rec drop n = function
  | [] -> []
  | head :: tails as xs -> delta (n = 0) xs (drop (n - 1) tails)
let tails xs = drop 1 xs

let rec take n = function 
  | [] -> []
  | head :: tails -> delta (n = 0) [] (head :: take (n - 1) tails)
let lizard xs = take (length xs - 1) xs

let manifold f xs trans default = match xs with
  | [] -> default
  | head :: tails -> fold_left (f trans) (trans head) tails

let caseOpt default = function None -> default | Some x -> x
let case x = caseOpt (failwith "case none") x

let explode s = init (String.length s) (String.get s)


(* torato *)

open Printf

let alphabets = explode "abcdefghijklmnopqrstuvwxyz"

module AdjacentMap = Map.Make (Char) ;;

Random.self_init () ;;
let randomVisit xs = nth xs (Random.int (length xs))
let next adjacents peek = peek

let string_of_pair f (x, y) = f x ^ f y
let string_of_char_pair = string_of_pair Char.escaped

(* AdjacentMap .mem x adjacent *)

let comma = fun trans s t -> s ^ ", " ^ trans t
let string_of_char_list xs = manifold comma xs Char.escaped "" ;;

let print_of_char_map key value = print_endline (Char.escaped key ^ ": " ^ string_of_char_list value) ;;

let adjacent = ref AdjacentMap . empty ;;

let map_find key = AdjacentMap.find key !adjacent ;;
let map_update key f = AdjacentMap.add key (f (map_find key)) !adjacent ;;
let map_print key = print_of_char_map key (AdjacentMap.find key !adjacent) ;;


adjacent := AdjacentMap.add 's' ['f'] !adjacent ;;
(* AdjacentMap.iter print_of_char_map !adjacent ;; *)

(* adjacent := map_update 'q' (fun v -> []) ;; *)
(* AdjacentMap.iter print_of_char_map !adjacent ;; *)

(* adjacent := AdjacentMap.(!adjacent |> add 'q' ['g'; 'm'; 't']) ;; *)


let handleToken token = 
  let xs = explode token in 
  let process index key =
    let succ = nth xs (index + 1) in
    let store () = map_update key (fun v -> succ :: v) in
    let add () = AdjacentMap.(!adjacent |> add key [succ]) in 
    let exists = AdjacentMap.mem key !adjacent in 
    adjacent := (delta exists store add) () in
    (* map_print key in *)
  iteri process (lizard xs) ;;

(* printf "%s " (string_of_char_pair (x, nth xs (index + 1))) *)

handleToken "mine" ;;

print_endline "---------------- data of map ----------------" ;;
AdjacentMap.iter print_of_char_map !adjacent ;;


(* iter (fun x -> printf "%c " (randomVisit alphabets)) [0; 0; 0] ;; *)
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







let rec mem x s = match s with
| [] -> false
| h :: t when x = h -> true
| h :: t -> mem x t;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;


let rec subseteq xl yl = match xl with
| [] -> true
| h :: t when (mem h yl) -> subseteq t yl
| _ -> false;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;


let seteq xl yl = (List.length xl = List.length yl) && subseteq xl yl;;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;


let rec dup l = match l with
| [] -> false
| h :: t when mem h t -> true
| h :: t -> dup t;; 

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;


let rec mkset l = if dup l then match l with
| [] -> []
| h :: t -> (if mem h t then [] else [h]) @ mkset t
else l;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;


let union xl yl = mkset (xl @ yl);;

let rec inter xl yl = match xl with
| [] -> []
| h :: t -> (if mem h yl then [h] else []) @ inter t yl;;

let rec diff xl yl = match xl with
| [] -> []
| h :: t -> (if mem h yl then [] else [h]) @ diff t yl;;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;

assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;

assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;


let dsum xl yl = (List.map(fun x -> (0, x)) xl) @ (List.map(fun x -> (1, x)) yl);;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;


let rec powset l = match l with
| [] -> [[]]
| h :: t ->
  let ps = powset t in (* ricorsivamente ottengo i sottinsiemi dell'insieme originale *)
  ps @ List.map (fun subset -> h :: subset) ps;; (* nel map ottengo tutte le sottoliste con h *)
  (* con 'ps @' aggiungo tutte le sottoliste ottenute fino a quel momento*)

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;
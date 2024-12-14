let mem x l = List.filter (fun y -> y = x) l |> List.length <> 0;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;

let subseteq xl yl = (List.filter (fun x -> mem x yl) xl) = xl;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;


let seteq xl yl = (subseteq xl yl) && (subseteq yl xl);;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;

let count x l = List.filter(fun y -> x = y) l |> List.length;;
let dup l = List.filter (fun x -> count x l = 1) l |> List.length <> List.length l;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;

let mkset l = List.fold_left (fun acc x -> if mem x acc then acc else acc @ [x]) [] l;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;

let union xl yl = mkset (xl @ yl);;

let inter xl yl = List.filter (fun x -> mem x yl) xl;;

let diff xl yl = List.filter (fun x -> not (mem x yl)) xl;;

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


let dsum xl yl = List.map (fun x -> (0, x)) xl @ List.map (fun x -> (1, x)) yl;; 

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;


let powset l = List.fold_left (fun acc x -> acc @ List.map (fun y -> y @ [x]) acc) [[]] l;;

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;
let rec count r c i = match r with
| [] -> c
| h :: t -> match (h mod 2) with 0 -> (match (i >= c) with true -> count t (i + 1) (i + 1) | _ -> count t c (i + 1)) | _ -> count t c 0;;

let consecutive_even l = count l 0 0;;


consecutive_even [1;2;3;4;5;6];;

assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);; 
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;
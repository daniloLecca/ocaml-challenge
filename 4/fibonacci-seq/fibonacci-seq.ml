(** 
  - length: number of elements of the Fibonacci sequence
  - i: index of the first element considered for the sum
  - j: index of the second element considered for the sum
  - l: list representing the current Fibonacci sequence
*)
let rec support length i j l = match length with
| n when n = 1 -> [0]
| n when n = 2 -> l
| _ -> support (length - 1) (i + 1) (j + 1) (l @ [(List.nth l i) + (List.nth l j)]);;

let fib n = support n 0 1 [0; 1];;

assert (fib 1 = [0]);;
assert (fib 2 = [0; 1]);;
assert (fib 3 = [0; 1; 1]);;
assert (fib 4 = [0; 1; 1; 2]);;
assert (fib 5 = [0; 1; 1; 2; 3]);;
assert (fib 6 = [0; 1; 1; 2; 3; 5]);;
assert (fib 7 = [0; 1; 1; 2; 3; 5; 8]);;
assert (fib 8 = [0; 1; 1; 2; 3; 5; 8; 13]);;
assert (fib 9 = [0; 1; 1; 2; 3; 5; 8; 13; 21]);;
assert (fib 10 = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]);;
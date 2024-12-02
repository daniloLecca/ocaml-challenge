let rec mem x l = match l with
| [] -> false
| h :: t -> (h = x) || (mem x t);;

let rec count0 = function
| [] -> 0
| h :: t -> (if h = 0 then 1 else 0) + count0 t;;

let rec count1 = function
| [] -> 0
| h :: t -> (if h = 1 then 1 else 0) + count1 t;;

let rec count11 l = match l with
| [] -> false
| [n] -> false
| h :: x :: t -> (h = 1 && x = 1) || count11 (x :: t);;


let rec l0 = function
| [] -> true
| [1] -> true
| [0] -> false
| h :: t -> l0 t;;

let rec l1 = function 
| [] -> true
| h :: t -> if ((h = 0) && not (mem h t))
                           then (count0 t) = 0
                           else l1 t;;


let rec l2 = function
| [] -> true
| h :: t -> if ((h = 1) && not (mem h t))
                           then (count0 t) = 0
                           else l2 t;;

let l3 l = (count0 l) = 0 || count11 l;;

let l4 l = (count0 l) >= (count1 l);;

let l5 l = (count0 l) = (count1 l);;

let rec supp n b = function
| [] -> n = 0 && b = 1
| 0 :: t -> if b = 1 then false else supp (n + 1) 0 t
| 1 :: t -> if n = 0 then false else supp (n - 1) 1 t
| _ -> false;;

let l6 l = l = [] || supp 0 0 l;;

let rec l7 n = function
| [] -> true
| 0 :: t -> l7 (n + 1) t
| 1 :: t -> n = count0 t
| _ -> false;;


let rec count2 = function
| [] -> 0
| h :: t -> (if h = 2 then 1 else 0) + count2 t;;

let rec l8 = function
| [] -> true
| h :: t -> if ((h = 1) && not (mem h t))
                           then (count0 t) = (count2 t)
                           else l8 t;;

(* parola suddivisibile in 2 parole tali che
    - Il numero di 0 nella prima è uguale al numero di 1 nella seconda
    - La prima parola non contiene 1
    - La seconda parola non contiene 0*)

(*s, v sono due subarray tali che s U v = l*)
let rec l9 l s v = match l with
| [] -> false
| h :: t -> (count0 (h :: s) = count1 t)
         && (count1 (h :: s) = 0)
         && (count0 t = 0) 
         || l9 l (h :: s) t;;

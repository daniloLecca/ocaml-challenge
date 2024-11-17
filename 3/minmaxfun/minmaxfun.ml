let rec maxfun f a b x max = match x with
| _ when x > b -> max
| _ -> if ((f x) > max) then maxfun f a b (x + 1) (f x) else maxfun f a b (x + 1) max;;

let rec minfun f a b x min = match x with
| _ when x > b -> min
| _ -> if ((f x) < min) then minfun f a b (x + 1) (f x) else minfun f a b (x + 1) min;;


let minmaxfun f a b = if (a >= b) then None else Some (minfun f a b a (f a), maxfun f a b a (f a));;


minmaxfun (function x -> x*x) 3 5;;

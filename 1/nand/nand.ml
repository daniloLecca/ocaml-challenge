let nand1 a b = not(a && b);;

nand1 false false;;


let nand2 a b = if (a && b) == false then true else false;;

nand2 false false;;


let nand3 a b = match (a, b) with
    (true, true) -> false
|    _ -> true;;

nand3 true true;
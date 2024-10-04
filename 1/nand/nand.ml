let nand1 a b = not(a && b);;

nand1 false false;;


let nand2 a b = if (a && b) == false then true else false;;

nand2 false false;;

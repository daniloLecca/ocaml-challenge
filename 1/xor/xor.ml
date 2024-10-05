let xor1 a b = ((not a ) && b) || (a && (not b));;

xor1 true false;;


let xor2 a b = if a == b then false else true;;

xor2 false true;;


let xor3 a b = match (a, b) with
| (false, false) -> false
| (true, true) -> false
| _ -> true;;

xor3 true false;;
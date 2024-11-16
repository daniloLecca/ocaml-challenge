type nat = Z | S of nat;;

let rec iseven a = match a with
| Z -> true
| S Z -> false
| S (S a) -> iseven a;;

let rec halve x = match x with
| Z -> Z
| S Z -> Z
| S (S x) -> S (halve x);;

let rec add a b = match (a, b) with
| (_, Z) -> a
| (_, S b) -> add (S a) (b);;

let rec mul a b = match (a, b) with
| (_, Z) -> Z
| (Z, _) -> Z
| (S x, _) -> add b (mul x b);;

let rec equals a b = match (a, b) with
| (Z, Z) -> true
| (Z, _) -> false
| (_ , Z) -> false
| (S a, S b) -> equals a b;;

let rec leq a b = match (a, b) with
| (Z, Z) -> true
| (Z, _) -> true
| (_ , Z) -> false
| (S a, S b) -> leq a b;;

type nat = Z | S of nat;;

let rec count x c = match c with
| 0 -> x
| _ -> count (S x) (c - 1);;

let nat_of_int x = count Z x;;

let rec convNat x c = match c with
| _ when (x = nat_of_int c) -> c
| _ -> convNat x (c + 1);;

let int_of_nat x = convNat x 0;;

int_of_nat (nat_of_int 7);;


let iseven x = if ((int_of_nat x) mod 2 = 0) then true else false;;

let halve x = if (iseven x) then nat_of_int ((int_of_nat x) / 2) else nat_of_int (((int_of_nat x) - 1) / 2);;

let add x y = nat_of_int((int_of_nat x) + (int_of_nat y));;

let mul x y = nat_of_int((int_of_nat x) * (int_of_nat y));;

let equals x y = (int_of_nat x) = (int_of_nat y);;

let leq x y = (int_of_nat x) <= (int_of_nat y);;
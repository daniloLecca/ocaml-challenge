let f1 (x: int) = 0;;
let f2 x = match x with 0 -> 1 | 1 -> 1 | 2 -> 1 | _ -> 0;;
let f3 x = match x with _ when x > 0 && x < 100 -> 1 | _ -> 0;;

let f4 x = 0;;

let f5 x = 1;;

let f6 x = 1 - (x mod 2);;

let f7 x = 1;;

let f8 x = 1;;

let f9 x = 1;;

let f10 x = match x > 7 with true -> 1 | _ -> 0;;

let f11 x = match (x < 50 && f6 x = 1) with true -> 1 | _ -> 0;;

let f12 z = f10 z;;

let f13 x = match x <= 20 with true -> 1 | _ -> 0;;
(*considerando naturali senza zero, se considero anche lo zero la funzione è 1 costante*)

let f14 x = 1;;

let f15 x = x mod 2;;
(*se x è divisibile per y allora y = 1 o y = x, cioè x è dispari*)

(*
∅
{0, 1, 2}
{x | 0 < x < 100}
{x | false }
{x | ∀y. x + y ≥ x}
{x | ∃y. x = y + y}
{x | ∃y. y = x + x}
{x | ∃y. x < y}
{x | ∃y. x * x = y}
{x | ∃y. y < 3 ∧ 7 < x y < 20 }
{x | x < 50 ∧ ∃y. x = y + y}
{z | ∃x. z = 2 x ∧ 0 < 2x < 50 }
{z | ∃x, y. z = x + y ∧ x y ≤ 20}
{x | ∀y. y < x → y < 2 }
{x | ∀y. (x mod y = 0) → (y=1 ∨ y=x) }
*)


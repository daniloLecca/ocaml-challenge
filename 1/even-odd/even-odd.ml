let is_even a = (a mod 2) = 0;;

let is_correct a = if (a >= 1 && a <= 5) then true else false;;

(*esempio di utilizzo di in: let a = 43 in the following expression, vuol dire che per 
l'espressione successiva c'è una variabile locale che vale 43*)

let win a b = match (is_correct a, is_correct b) with
| (false, false) -> 0
| (false, true) -> -1
| (true, false) -> 1
| (true, true) -> match is_even (a + b) with
| true -> 1
| _ -> -1;;

(*
- 1, if the first player wins (i.e., the one who played the number `a`);
- -1, if the second player wins;
- 0, if no one wins.*)
type definition = Some of int | Undefined | None;;

let two_equals (a, b, c) = (a = b || b = c || a = c);;
let equal_number (a, b, c) = match (a, b, c) with
| _ when (a = b || a = c) -> a
| _ when (b = c) -> b
| _ -> 0
;;

let consensus3 (f1, f2, f3) argument = if argument = 0 then Undefined else match (f1 argument, f2 argument, f3 argument) with
| _ when argument = 0 -> Undefined
| _ when two_equals (f1 argument, f2 argument, f3 argument)-> Some (equal_number (f1 argument, f2 argument, f3 argument))
| _ -> None;;

consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0 = Undefined;;
consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5;;
consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2;;
consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None;;
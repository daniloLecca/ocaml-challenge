let f x = match x with
| None -> 0
| Some x -> x + 1;;

let op x = Some x;;

let incr_opt x = match x with
| None -> None
| x -> op (f x);;

incr_opt (Some 4);;
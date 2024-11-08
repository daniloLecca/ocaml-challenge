let f1 x = x >= 0;;

let f2 x = if x then 1 else 0;;

let f3 x = (x, x >= 0);;

let f4 (x, y) = if y then x else -x;;

let f5 x = fun y -> x + y;;

let f6 x = fun y -> if y + x >= 0 then true else false;;

let f7 x = fun y -> if (x || y >= 0) then true else false;;

let f8 x = fun y -> if (x || y ) then 1 else 0;;

let f9 x = fun y -> if (x || y >= 0) then 1 else 0;;

let f10 f = (f 0) + 1;;

let f11 f = if (f 0) then 1 else 0;;

let f12 f = if ((f true) >= 0) then 1 else 0;;

let f13 f = if (f 0) then false else true;;

let f14 f = if (f false) then 0 else 1;;

let f15 x (a, b) = x * (a + b);;

let f16 x = fun y -> fun z -> x + y + z;;

let f17 f = match (f 0) with 0 -> f | _ -> f;;

let f18 f = fun g -> fun x -> f (g (x + 1) - 1) + 1;;


(*
f1 : int -> bool
f2 : bool -> int
f3 : int -> (int * bool)
f4 : (int * bool) -> int
f5 : int -> (int -> int)
f6 : int -> (int -> bool)
f7 : bool -> (int -> bool)
f8 : bool -> (bool -> int)
f9 : bool -> (int -> int)
f10 : (int -> int) -> int
f11 : (int -> bool) -> int
f12 : (bool -> int) -> int
f13 : (int -> bool) -> bool
f14 : (bool -> bool) -> int
f15 : int -> (int * int) -> int
f16 : int -> (int -> (int -> int))
f17 : (int -> int) -> (int -> int)
f18 : ((int -> int) -> int) -> int
*)
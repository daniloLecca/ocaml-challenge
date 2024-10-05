let mux2 s0 a b = if s0 then a else b;;


let mux4 s0 s1 a0 a1 a2 a3 = match (s0, s1, a0, a1, a2, a3) with
| (false , false, _, _, _, _) -> a0
| (false , true, _, _, _, _) -> a1
| (true , false, _, _, _, _) -> a2
| (true , true, _, _, _, _) -> a3;;

assert(mux4 false false false true false true = false);;
assert(mux4 false true false true false true = true);;
assert(mux4 true false false true false true = false);;
assert(mux4 true true false true false true = true);;

let mux5 s0 s1 a0 a1 a2 a3 = mux2 s0 (mux2 s1 a3 a2) (mux2 s1 a1 a0);;

assert(mux5 false false false true false true = false);;
assert(mux5 false true false true false true = true);;
assert(mux5 true false false true false true = false);;
assert(mux5 true true false true false true = true);;
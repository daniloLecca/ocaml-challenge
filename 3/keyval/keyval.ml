let apply f k = let filtered = List.filter (fun x -> (fst x) = k) f in match filtered with [] -> None | (a, b)::t -> Some b;;


let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)];;
assert(apply f0 4 = Some 5);;
assert(apply f0 6 = None);;
assert(apply f0 2 = Some 3);;
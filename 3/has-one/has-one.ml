let rec has_one x = match x with
| n when n < 0 -> failwith "only positive integers"
| n when n = 0 -> false
| n when n mod 10 = 1 -> true
| _ -> has_one (x / 10);;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;


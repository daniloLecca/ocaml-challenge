let is_posfrac (a, b) = if b = 0 then failwith "Not a fraction" else a * b > 0;;

let compare_posfrac (a, b) (c, d) = if (is_posfrac (a, b) && is_posfrac (c, d)) then match (a, b) with
| _ when a * d > c * b -> 1
| _ when a * d < c * b -> -1
| _ -> 0
else failwith "Not a fraction";;

assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;

let compare_frac (a, b) (c, d) = match (is_posfrac (a, b), is_posfrac (c, d)) with
| (false, false) -> -1 * compare_posfrac (-a, b) (-c, d)
| (false, true) -> -1
| (true, false) -> 1
| (true, true) -> compare_posfrac (a, b) (c, d);;


assert (compare_frac (-1,2) (2,-4) == 0);;
assert (compare_frac (1,2) (1,-3) == 1);;
assert (compare_frac (1,2) (2,3) == -1);;
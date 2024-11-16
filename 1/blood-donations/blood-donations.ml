type blood_group = A | B | AB | O;;

let check_groups x y = match (x, y) with
| (O, _) -> true
| (A, A) -> true
| (A, AB) -> true
| (B, B) -> true
| (B, AB) -> true
| (AB, AB) -> true
| _ -> false;;

assert (check_groups O AB = true);;
assert (check_groups A B = false);;
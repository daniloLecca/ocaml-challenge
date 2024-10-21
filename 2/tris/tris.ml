let hand = (Random.int(9), Random.int(9), Random.int(9), Random.int(9));;

let tris (a, b, c, d) = match (a, b, c, d) with
| _ when (a = b && a = c) -> true
| _ when (a = b && a = d) -> true
| _ when (b = c && b = d) -> true
| _ when (a = c && a = d) -> true
| _ -> false;;

tris hand;;
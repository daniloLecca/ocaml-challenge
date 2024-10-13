let sum_ratings a b c = a + b + c;;

let check_rating a = a >= 1 && a <= 5;;

let movie_rating a b c = match sum_ratings a b c with
| 15 -> "Masterpiece"
| 14 -> "Highly Recommended"
| _ when (sum_ratings a b c >= 11) && (a = 3 || b = 3 || c = 3) -> "Recommended"
| _ when check_rating a && check_rating b && check_rating c-> "Mixed Reviews"
| _ -> failwith "Invalid ratings";;

movie_rating 5 2 4;;
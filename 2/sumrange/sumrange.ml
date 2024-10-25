let rec sumrange a b = match a, b with
| _ when a > b -> 0
| _ -> a + sumrange (a + 1) b;;

assert (sumrange 0 1 = 1);;

assert (sumrange 1 3 = 6);;

assert (sumrange 3 2 = 0);;
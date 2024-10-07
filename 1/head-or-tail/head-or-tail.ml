let hot n = let random = Random.int(n) in match random with
| 0 -> "head"
| _ -> "tails";;

hot 2;;
let guess5 n = if (n >= 1 && n <= 5) then let r = Random.int(5) + 1 in (r == n, r) else failwith("");;

guess5 2;;
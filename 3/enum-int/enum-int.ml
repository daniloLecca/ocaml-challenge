let enum_int a = match ((a mod 2) = 0) with true -> a / 2 | _ -> -((a / 2) + 1);;

assert (List.init 10 enum_int = [0; -1; 1; -2; 2; -3; 3; -4; 4; -5]);;
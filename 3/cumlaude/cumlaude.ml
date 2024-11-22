type grade = Val of int | CumLaude;;

let is_valid = function Val (n) when n >= 18 && n <= 30 -> true | CumLaude -> true | _ -> false;;

let int_of_grade gr = if (is_valid gr) then (match gr with Val (n) -> n | CumLaude -> 32) else failwith "grade not valid";;

let avg l = (List.fold_left (fun acc x -> acc + int_of_grade x) 0 l) / List.length l;;

let avg_norec l = List.filter is_valid l |> avg;;
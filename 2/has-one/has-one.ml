let rec digits_of_int number = match number with
| _ when number < 10 -> [number]
| _ -> digits_of_int (number / 10) @ [number mod 10];;

let int_of_digits digits =
  List.fold_left (fun acc digit -> acc * 10 + digit) 0 digits;;


let rec has_one number = match digits_of_int number with
| _ when number < 0 -> failwith "Positives only"
| [_] when (number < 10 && number != 1) -> false
| [] -> false
| h :: t -> match h with 1 -> true | _ -> has_one (int_of_digits t);;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;


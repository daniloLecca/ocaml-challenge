let max_offer a b = if a > b then a else b;;

let to_int a = match a with
| Some a -> a
| None -> 0;;

let best_offer a b c = match (a, b, c) with
| (None, None, None) -> None
| _ -> Some (max_offer(max_offer(to_int a) (to_int b)) (to_int c));;


best_offer (Some 100) (Some 200) (Some 150) = Some 200;;

best_offer (Some 100) None (Some 150) = Some 150;;

best_offer None None None = None;;

best_offer None (Some 300) None = Some 300;;


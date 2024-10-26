let rec countzero f a b = match a, b with
| _ when a > b -> 0
| _ when f a = 0  -> 1 + countzero f (a + 1) b
| _ -> countzero f (a + 1) b;;


countzero (fun x -> x*x -4) (-5) (3);;


assert (countzero (fun x -> x) (-10) 10 = 1);;

assert (countzero (fun x -> x) 1 10 = 0);;

assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;

assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;
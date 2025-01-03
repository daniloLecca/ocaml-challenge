(**
 Returns a list containing the digits of the number (in inverse order)
*)
let rec list_of_int n = match n with
| a when a < 10 -> [a]
| _ -> [n mod 10] @ list_of_int (n / 10);;

(**
  - n: number to be checked
*)
let alt_even n = list_of_int n |> List.mapi (fun i x -> (x mod 2) = (i mod 2)) |> List.for_all (fun x -> x);;
(* takes the number -> converts to list of digits ->
   checks if both the element and the index are even or odd ->
   checks if all the elements of the list are true
*)

assert (alt_even 8);;
assert (alt_even 72);;
assert (alt_even 1234);;
assert (alt_even 3 = false);;
assert (alt_even 51 = false);;
assert (alt_even 8234 = false);;
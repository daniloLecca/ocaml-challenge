let rec gcd a b = if (a < 0 || b < 0) then failwith "only positive integers" else
   match b with
| 0 -> a
| _ -> gcd b (a mod b);;

gcd 10 2;;
  
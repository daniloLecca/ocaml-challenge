let first_third_fifth l = match l with a :: b :: c :: d :: e :: t when List.length l >= 5 -> Some (a, c, e) | _ -> None;;

assert(first_third_fifth ["cat"; "dog"] = None);;
assert(first_third_fifth [1; 2; 3; 4; 5; 6] = Some (1, 3, 5));;
   


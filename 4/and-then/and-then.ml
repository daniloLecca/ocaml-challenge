let ( -?-> ) (o : 'a option) (next : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some x -> next x;;

let first_third_fifth_original l = if List.length l < 5 then None else match l with a :: b :: c :: d :: e :: t -> Some (a, c, e) | _ -> None;;

let first_third_fifth_redefined l = List.nth_opt l 0 -?-> fun x -> List.nth_opt l 2 -?-> fun y -> List.nth_opt l 4 -?-> fun z -> Some (x, y, z);;


assert(first_third_fifth_original ["cat"; "dog"] = None);;
assert(first_third_fifth_original [1; 2; 3; 4; 5; 6] = Some (1, 3, 5));;

assert(first_third_fifth_redefined ["cat"; "dog"] = None);;
assert(first_third_fifth_redefined [1; 2; 3; 4; 5; 6] = Some (1, 3, 5));;

(* the two definitions are quite of the same length*)
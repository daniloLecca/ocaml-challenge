let rec str l = match l with
| [] -> ""
| h :: t -> (string_of_int h) ^ (match t with [] -> "" | _ -> ";") ^ (str t);;

let string_of_list l = "[" ^ str l ^ "]";;

string_of_list [1; 2; 3];;

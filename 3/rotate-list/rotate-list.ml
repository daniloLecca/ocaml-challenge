let rotate l = match l with
| [] -> []
| h :: t -> t @ [h] ;;

rotate [1; 2; 3];;


(* i: indice dell'elemento da estrarre
   l: lista iniziale
   a: lista degli elementi già letti*)

let rec ext i l a = match l with
| [] -> failwith "index out of bounds"
| h :: t -> if i = 0 then (h, a @ t) else ext (i - 1) (t) (a @ [h]);;

let extract i l = ext i l [];;


assert (extract 0 [1;2;3] = (1, [2; 3]));;

assert (extract 1 [1;2;3] = (2, [1; 3]));;

assert (extract 2 [1;2;3] = (3, [1; 2]));;

extract 3 [1;2;3];;
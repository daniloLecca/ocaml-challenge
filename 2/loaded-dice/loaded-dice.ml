let rand = Random.int(101);;

let dice a = match rand with
| _ when rand <= a -> 6
| _ when rand <= a + (100 - a)/5 -> 1
| _ when rand <= a + 2*(100 - a)/5 -> 2
| _ when rand <= a + 3*(100 - a)/5 -> 3
| _ when rand <= a + 4*(100 - a)/5 -> 4
| _  -> 5;;

(*assegno una zona di probabilità possibile a 6, con zona [1; a]*)
(*i restanti numeri possibili (100 - a) li suddivido per i restanti numeri del dado*)

dice 80;;
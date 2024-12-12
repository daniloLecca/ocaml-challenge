type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let rec mem x = function 
| [] -> false
| h :: t -> h = x || mem x t;;

let rec ext i l a = match l with
| [] -> failwith "index out of bounds"
| h :: t -> if i = 0 then (h, a @ t) else ext (i - 1) (t) (a @ [h]);;

let extract i l = ext i l [];;


let rec is_complete deck = match deck with
| [] -> true
| h :: t -> if mem h t then false else is_complete t

let rec gen_static_complete_deck i = match i with
| n when n >= 0 && n <= 10 -> [Card (i, Spades); Card (i, Hearts); Card (i, Diamonds); Card (i, Clubs)] @ gen_static_complete_deck (i + 1)
| _ -> [];;
let rec support originalDeck i = match i with
| n when n > 0 -> let l' = extract (Random.int i) originalDeck in ([fst l'] @ support (snd l') (i - 1))
| _ -> [];;

let gen_deck = support (gen_static_complete_deck 1) 40;;

assert (is_complete gen_deck);;
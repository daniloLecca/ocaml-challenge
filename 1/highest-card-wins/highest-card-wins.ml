type card = Joker | Val of int;;

let win player dealer = match (player, dealer) with
| (Joker, Joker) -> false
| (Joker, _) -> true
| (_, Joker) -> false
| _ -> player > dealer;;

win (Val 3) (Val 7);;
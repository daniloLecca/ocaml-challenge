type suit = S | H | D | C;;
type card = Card of int * suit;;

let getIntCard a = match a with Card(value, _) -> value;;

let straight (a, b, c, d, e) = match (a, b, c, d, e) with
| _ -> getIntCard a <= getIntCard b && getIntCard b <= getIntCard c && getIntCard c <= getIntCard d && getIntCard d <= getIntCard e;;

straight (Card(1, S), Card(3, D), Card(6, H), Card(8, S), Card(9, C));;


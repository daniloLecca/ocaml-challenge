type suit = S | H | D | C;;
type card = Card of int * suit;;

let random_suit a = match Random.int(4) with
| 0 -> S
| 1 -> H
| 2 -> D
| _ -> C;;

let getIntCard a = match a with Card(value, _) -> value;;
let getSuitCard a = match a with Card(_, suit) -> suit;;

let checkDifferentSuits a b c d = (getSuitCard a != getSuitCard b && getSuitCard b != getSuitCard c && getSuitCard c != getSuitCard d);;

let rndHand = (Card(Random.int(10), random_suit 0), Card(Random.int(10), random_suit 1), Card(Random.int(10),
               random_suit 2), Card(Random.int(10), random_suit 3), Card(Random.int(10), random_suit 4));;

let poker (a, b, c, d, e) = match (a, b, c, d, e) with
| _ when (getIntCard a = getIntCard b && getIntCard a = getIntCard c && getIntCard a = getIntCard d) -> checkDifferentSuits a b c d
| _ when (getIntCard a = getIntCard b && getIntCard a = getIntCard c && getIntCard a = getIntCard e) -> checkDifferentSuits a b c e
| _ when (getIntCard a = getIntCard b && getIntCard a = getIntCard d && getIntCard c = getIntCard e) -> checkDifferentSuits a b d e
| _ when (getIntCard a = getIntCard c && getIntCard a = getIntCard d && getIntCard a = getIntCard e) -> checkDifferentSuits a c d e 
| _ when (getIntCard b = getIntCard c && getIntCard b = getIntCard d && getIntCard b = getIntCard e) -> checkDifferentSuits b c d e
| _ -> false;;

poker rndHand;;
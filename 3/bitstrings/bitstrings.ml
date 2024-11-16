type bitstring = E | Z of bitstring | U of bitstring;;

let rec string_of_bitstring str = match str with
| E -> ""
| Z str -> "0" ^ string_of_bitstring str
| U str -> "1" ^ string_of_bitstring str;;

let rec len bits = match bits with
| E -> 0
| Z bits -> 1 + len bits
| U bits -> 1 + len bits;;

let rec countZ bits = match bits with
| E -> 0
| Z bits -> 1 + countZ bits
| U bits -> countZ bits;;

let rec countU bits = match bits with
| E -> 0
| Z bits -> countU bits
| U bits -> 1 + countU bits;;

let rec support a b = match (a, b) with
| (E, E) -> E
| (E, _) -> b
| (_, E) -> a
| (_, Z b) -> support (Z a) (b)
| (_, U b) -> support (U a) (b);;

let concat a b = support b a;;

let rec equals a b = match (a, b) with
| (E, E) -> true
| (E, _) -> false
| (_, E) -> false
| (Z a, Z b) -> equals a b
| (U a, U b) -> equals a b
| _ -> false;;

let rec tl bits = match bits with
| E -> E
| Z bits -> bits
| U bits -> bits;;

let rec prefix a b = match (a, b) with
| (E, _) -> true
| (Z a, Z b) -> prefix a b
| (U a, U b) -> prefix a b
| _ -> false;;

let rec subs a b originalSubstring originalString = match (a, b) with
| (E, _) -> true (* la substring è già stata 'scorsa', quindi ha già fatto match*)
| (_, E) -> false (* la stringa è finita e il match non è stato trovato, la substring non è presente*)
| (Z a, Z b) -> subs (a) (b) (originalSubstring) (originalString)
| (U a, U b) -> subs (a) (b) (originalSubstring) (originalString) 
| _ -> match originalString with
  | E -> false
  | Z originalString -> subs originalSubstring originalString originalSubstring originalString
  | U originalString -> subs originalSubstring originalString originalSubstring originalString;;

let substring a b = subs a b a b;;


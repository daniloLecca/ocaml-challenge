let rec merge l m = match l, m with
| h :: t, [] -> l
| [], h :: t -> m
| [], [] -> []
| h :: t, h' :: t' -> if h < h' then h :: merge t m else h' :: merge l t';;

let rec knife l i =
  if i <= 0 then ([], l)
  else match l with
    | [] -> ([], [])
    | h :: t -> 
      let (l, r) = knife t (i - 1) in
      (h :: l, r);;

let halve l = knife l ((List.length l) / 2);;

assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);;
assert (merge [7] [2;3;6] = [2;3;6;7]);;
assert (merge [7] [] = [7]);;

assert (halve [1;3;5;8;-2;6] = ([1;3;5], [8;-2;6]));;
assert (halve [1;3] = ([1], [3]));;
assert (halve [1;3;5] = ([1], [3;5]));;


let rec merge_sort l = match halve l with
| ([], []) -> []
| ([], _) -> l
| (_, []) -> l
| (sl, sl') -> merge (merge_sort sl) (merge_sort sl');;

assert (merge_sort [1;3;5;8;-2;6] = [-2;1;3;5;6;8]);;
let rec knife l i =
  if i <= 0 then ([], l)
  else match l with
    | [] -> ([], [])
    | h :: t -> 
      let (l, r) = knife t (i - 1) in
      (h :: l, r);;

assert (knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));;
assert (knife ['b';'r';'e';'a';'d'] 3  = (['b';'r';'e'], ['a';'d']));;
assert (knife [] 0 = ([], []));;
assert (knife ["miss"; "me"] 2  = (["miss"; "me"], []));;
assert (knife ["oops"] (-1)  = ([], ["oops"]));;
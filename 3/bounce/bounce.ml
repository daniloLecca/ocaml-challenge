let bounce n x = let res = x mod (2 * n) in
  match res with
  | _ when res <= n -> res
  | _ -> 2 * n - res;;

let foo = bounce 3;;

foo 1;;
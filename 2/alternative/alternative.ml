let (<|>) x y = match (x, y) with
| (None, None) -> None
| (None, _) -> y
| (_, None) -> x
| _ -> x;;

assert (Some true <|> Some false = Some true);;
assert (None <|> Some false = Some false);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
assert (None <|> Some "dog" = Some "dog");;
let in_range x a b = if int_of_char(x) >= int_of_char(a) && int_of_char(x) <= int_of_char(b) then true else false;;

in_range '3' '2' '9';;
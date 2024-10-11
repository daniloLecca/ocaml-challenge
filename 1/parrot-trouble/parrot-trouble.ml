let parrot_trouble talking hour = match talking with
| true when hour < 7 || hour > 23 -> Some true
| _ -> None;;

parrot_trouble true 2;;
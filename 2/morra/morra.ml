type winner = Player | Computer | Tie ;;

let randomHandComputer () = (Random.int(6), Random.int(11));;

(*Eseguo il match sulla base della somma delle mani dei giocatori*)
let win (hp, gp) = let computer = randomHandComputer () in match (fst (computer) + hp) with 
| _ when ((gp = (fst (computer) + hp)) &&  (snd (computer) != (fst (computer) + hp))) -> (computer, Player)
| _ when ((gp != (fst (computer) + hp)) &&  (snd (computer) = (fst (computer) + hp))) -> (computer, Computer)
| _ -> (computer, Tie);;

win (4, 7);;

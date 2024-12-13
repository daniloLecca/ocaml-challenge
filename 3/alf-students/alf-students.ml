type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
};;


let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

let rec id_of_noshow l = match l with
| [] -> []
| h :: t -> (if h.vote = None then [h.id] else []) @ id_of_noshow t;;

let rec upgradeable l = match l with
| [] -> []
| h :: t -> (if h.vote >= Some 15 && h.vote <= Some 17 then [h.name] else []) @ upgradeable t;;

let rec upgrade l = match l with
| [] -> []
| h :: t -> (if h.vote >= Some 15 && h.vote <= Some 17 then [{h with vote = Some 18}] else [h]) @ upgrade t;;

let rec wrong_laude l = match l with
| [] -> []
| h :: t -> (if h.laude && h.vote <> Some 30 then [h.name] else []) @ wrong_laude t;;

let rec fix_laude l = match l with
| [] -> []
| h :: t -> (if h.vote <> Some 30 then [{h with laude = false}] else [h]) @ fix_laude t;;

let students_passed l = List.filter(fun x -> x.vote >= Some 18) l;;

let percent_passed l = ((List.length (students_passed l)) * 100) / List.length l;;

let sum_without_laude l = students_passed l |>
                 List.map (fun x -> match x.vote with Some v -> v | _ -> 0) |> 
                 List.fold_left (fun acc x -> acc + x) 0 ;;

let rec sum_laude l =  2 * List.length (List.filter(fun x -> x.laude) l);; 
(* ad ogni studente che ha preso la lode vengono dati due punti in più*)

let avg_vote l = float_of_int(((sum_without_laude l) + (sum_laude l)) / (List.length (students_passed l)));;
(* somma dei voti assegnati / numero di persone che hanno passato l'esame*)
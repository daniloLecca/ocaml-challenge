type ('a,'b) fsa = {
  trans: ('a * 'b * 'a) list;      (* set of transitions *)
  init: 'a;                        (* initial state *)
  final: 'a list                   (* final states *)
};;

let rec subseteq xl yl = match xl with
    [] -> true
  | x::xl' -> List.mem x yl && subseteq xl' yl;;

let seteq xl yl = (subseteq xl yl) && (subseteq yl xl);;

(* m1 deterministic and complete *)
let m1 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2);(2,'1',2)];
  init = 0;
  final = [1] }
;;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

(* m3 deterministic and non-complete *)
let m3 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',1);(1,'1',2)];
  init = 0;
  final = [1;2] }
;;

let rec support_getlabels xl = match xl with
| [] -> []
| (_, l, _) :: t -> l :: support_getlabels t;;

let rec getlabels m = m.trans |> support_getlabels;;

assert (seteq (getlabels m1) ['0';'1']);;
assert (seteq (getlabels m2) ['0';'1']);;
assert (seteq (getlabels m3) ['0';'1']);;



let rec support_outlabels state xl = match xl with
| [] -> []
| (s, l, _) :: t -> (if s = state then [l] else []) @ support_outlabels state t;;

let rec outlabels m q = support_outlabels q m.trans;;

assert (seteq (outlabels m1 0) ['0';'1']);;
assert (seteq (outlabels m1 1) ['0';'1']);;
assert (seteq (outlabels m1 2) ['0';'1']);;
assert (seteq (outlabels m2 2) ['0']);;



let mkset l = List.fold_left (fun acc x -> if List.mem x acc then acc else acc @ [x]) [] l;;

let rec support_getstates xl = match xl with
| [] -> []
| (s, l, s') :: t -> s :: s' :: support_getstates t;;

let getstates m = m.trans |> support_getstates |> mkset;;

assert (seteq (getstates m1) [0;1;2]);;
assert (seteq (getstates m2) [0;1;2]);;
assert (seteq (getstates m3) [0;1;2]);;




let rec support_iscomplete states labels m = match states with
| [] -> true
| h :: t -> (seteq (outlabels m h) (labels)) && support_iscomplete t labels m;;

let is_complete m = support_iscomplete (getstates m) (getlabels m) m;;

assert (is_complete m1);;
assert (is_complete m2 = false);;
assert (is_complete m3 = false);;

(* given an initial state and a label, returns the list of reachable states*)
let rec arriving_states_given_label start_state label transitions = match transitions with
| [] -> []
| (s, l, s') :: t -> (if (s = start_state && l = label) then [s'] else []) @ arriving_states_given_label start_state label t;;

(* given an initial state and a set of labels, returns true if the transition from the initial state is deterministic*)
let rec support_is_deterministic start_state labels transitions = match labels with
| [] -> true
| h :: t -> (arriving_states_given_label start_state h transitions |> List.length <= 1) && support_is_deterministic start_state t transitions;;

(* for every state checks if for every label the number of reachable states is at most 1 *)
let is_deterministic m = List.for_all (fun state -> support_is_deterministic state (getlabels m) (m.trans)) (getstates m);;

assert (is_deterministic m1);;
assert (is_deterministic m2 = false);;
assert (is_deterministic m3);;



let rec support_step1 start_state label transitions = match transitions with
| [] -> failwith "transition not found"
| (s, l, s') :: t -> if (s = start_state && l = label) then s' else support_step1 start_state label t;;

let step1 q a m = support_step1 q a m.trans;; 

assert (step1 0 '0' m1 = 0);;
assert (step1 0 '1' m1 = 1);;
assert (step1 1 '0' m1 = 2);;
assert (step1 1 '1' m1 = 2);;
assert (step1 2 '0' m1 = 2);;
assert (step1 2 '1' m1 = 2);;




let rec support_step start_state word fsa = match word with
| [] -> start_state
| h :: t -> support_step (step1 start_state h fsa) t fsa;;

let step q w m = support_step q w m;;

assert(step 0 ['0';'0';'0'] m1 = 0);;
assert(step 0 ['0';'1';'1'] m1 = 2);;




let accept w m = List.mem (step (m.init) w m) m.final;;

assert (accept ['0';'0';'1'] m1);;
assert (accept ['0';'0';'1';'1'] m1 = false);;
assert (accept ['1';'0';'0';'1'] m1 = false);;

(* for every state check if *)

(* difference operation for sets*)
let diff xl yl = List.filter (fun x -> not (List.mem x yl)) xl;;

(* returns the list of label readable from a certain state*)
let rec support_getlabels_given_state state transitions = match transitions with
| [] -> []
| (s, l, _) :: t -> (if s = state then [l] else []) @ support_getlabels_given_state state t;;

let getlabels_given_state state m = support_getlabels_given_state state m.trans |> mkset;;

(* returns a list of type [[l1, l2], [], [l1], ...], where l is the missing label from the corresponding state in the list *)
let rec support_missing_labels states labels m = match states with
| [] -> []
| h :: t -> let l = mkset (getlabels m) in let l' = mkset (getlabels_given_state h m) in [diff l l'] @ support_missing_labels t labels m;;

let missing_labels m = support_missing_labels (getstates m) (getlabels m) m;;

(* for each state add the corresponding missing label with the sink state*)
let rec add_sink_transitions states missing_ls = match states, missing_ls with
  | [], [] -> []
  | h :: t, l :: lt -> (List.map (fun label -> (h, label, -1)) l) @ add_sink_transitions t lt
  | _, _ -> failwith "states and labels have mismatched";;

let new_transitions m = add_sink_transitions (getstates m) (missing_labels m);;

(* add the loop transitions of the sink state*)
let sink_loops m = List.map (fun label -> (-1, label, -1)) (getlabels m);;
let complete m = { m with trans = m.trans @ new_transitions m @ sink_loops m };;
let m3' = complete m3;;
assert (is_complete m3');;
assert (accept ['0';'1';'0';'1'] m3');;
assert (accept ['0';'0';'1';'0';'0'] m3');;
assert (accept ['0';'1';'1';'0'] m3' = false);;
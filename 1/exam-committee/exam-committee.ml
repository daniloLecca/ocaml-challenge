type vote = StrongAccept | StrongReject | WeakAccept | WeakReject

let rejected a = match a with
| StrongReject -> true
| _ -> false;;

let decide_exam a b c = match (a, b, c) with
| _ when rejected a || rejected b || rejected c -> false
| (WeakReject, WeakReject, _) -> false
| (WeakReject, _, WeakReject) -> false
| (_, WeakReject, WeakReject) -> false
| _ -> true;;

decide_exam WeakReject WeakReject WeakAccept;;
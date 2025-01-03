type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let is_left_valid left_tree value f = match left_tree with
  | Empty -> true
  | Node (left_value, _, _) -> compare left_value value <= 0;;

let is_right_valid right_tree value f = match right_tree with
 | Empty -> true
 | Node (right_value, _, _) -> compare right_value value >= 0;;

(*checks if a tree is a binary search tree *)
let rec is_bstree tree f = match tree with
  | Empty -> true
  | Node (value, lt, rt) -> is_left_valid lt value f && is_right_valid rt value f && is_bstree lt f && is_bstree rt f;;


let example_tree = Node(7, Node(4, Node(1,Empty,Empty), Node(5,Empty,Empty)), Node(10,Empty,Empty));;

assert (is_bstree example_tree compare);;



let rec search tree f x = match tree with
| Empty -> false
| Node (value, lt, rt) ->
   (if compare value x > 0 then search lt f x
   else if compare value x < 0 then search rt f x
   else true);;

assert (search example_tree compare 5);;
assert (search example_tree compare 4);;
assert (search example_tree compare 10);;
assert (search example_tree compare 7);;
assert (search example_tree compare 1);;
assert (search example_tree compare 2 = false);;
assert (search example_tree compare 0 = false);;
assert (search example_tree compare 12 = false);;

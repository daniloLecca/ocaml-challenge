let enum_nat_nat a =
  let k = int_of_float ((sqrt (8.0 *. float_of_int a +. 1.0) -. 1.0) /. 2.0) in
  let i = a - (k * (k + 1)) / 2 in (i, k - i);;

enum_nat_nat 15;;


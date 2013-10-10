let rec tak x y z =
  if y >= x then z else
  tak (tak (x -. 1.0) y z) (tak (y -. 1.0) z x) (tak (z -. 1.0) x y) in
let n = 10.0 in
print_int (int_of_float (1000000.0 *. tak (n *. 3.0) (n *. 2.0) (n *. 1.0)));
print_newline ()

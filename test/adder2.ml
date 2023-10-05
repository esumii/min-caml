let rec make_adder x =
  let rec addx y = x + y in
  addx in
let add2 = make_adder 2 in
let add3 = make_adder 3 in
print_int (add2 10 + add3 20)

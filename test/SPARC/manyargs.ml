(* thanks to https://twitter.com/gan13027830/status/791239623959687168 *)
(* exactly one more arguments than registers; does not copmile *)
let x = 42 in
let rec f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 = print_int (x + y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10 + y11 + y12 + y13 + y14 + y15 + y16) in
f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16

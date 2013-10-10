(*MINCAML*) let rec f n i d s =
(*MINCAML*)   if i > n then s else
(*MINCAML*)   f n (i + 1) (d +. 1.0) (s +. 1.0 /. d) in
(*MINCAML*) print_int (int_of_float (1000000.0 *. f 100000000 2 2.0 1.0));
(*NOMINCAML let f n = *)
(*NOMINCAML   let s = ref 1.0 in *)
(*NOMINCAML   let d = ref 2.0 in *)
(*NOMINCAML   for i = 2 to n do *)
(*NOMINCAML     s := !s +. 1.0 /. !d; *)
(*NOMINCAML     d := !d +. 1.0 *)
(*NOMINCAML   done; *)
(*NOMINCAML   !s +. 0.0 in *)
(*NOMINCAML print_int (int_of_float (1000000.0 *. f 100000000)); *)
print_newline ()

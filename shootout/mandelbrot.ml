(*MINCAML*) let rec dbl f = f +. f in
(*NOMINCAML let dbl f = 2. *. f in *)
(*NOMINCAML for y = 0 to 399 do *)
(*MINCAML*) let rec yloop y =
(*MINCAML*)   if y >= 400 then () else
(*NOMINCAML   for x = 0 to 399 do *)
(*MINCAML*)   let rec xloop x y =
(*MINCAML*)     if x >= 400 then () else
                let cr = dbl (float_of_int x) /. 400.0 -. 1.5 in
                let ci = dbl (float_of_int y) /. 400.0 -. 1.0 in
                let rec iloop i zr zi zr2 zi2 cr ci =
                  if i = 0 then print_int 1 else
                  let tr = zr2 -. zi2 +. cr in
                  let ti = dbl zr *. zi +. ci in
                  let zr = tr in
                  let zi = ti in
                  let zr2 = zr *. zr in
                  let zi2 = zi *. zi in
                  if zr2 +. zi2 > 2.0 *. 2.0 then print_int 0 else
                  iloop (i - 1) zr zi zr2 zi2 cr ci in
                iloop 1000 0.0 0.0 0.0 0.0 cr ci;
(*
                let i = ref 1000 in
                let zr = ref 0.0 in
                let zi = ref 0.0 in
                let zr2 = ref 0.0 in
                let zi2 = ref 0.0 in
                while (if !i = 0 then (print_int 1; false) else
                       let tr = !zr2 -. !zi2 +. cr in
                       let ti = dbl !zr *. !zi +. ci in
                       zr := tr;
                       zi := ti;
                       zr2 := !zr *. !zr;
                       zi2 := !zi *. !zi;
                       if !zr2 +. !zi2 > 2.0 *. 2.0 then (print_int 0; false) else
                       true) do
                  i := !i - 1
                done;
*)
(*MINCAML*)     xloop (x + 1) y in
(*MINCAML*)   xloop 0 y;
(*NOMINCAML   done; *)
(*MINCAML*)   yloop (y + 1) in
(*MINCAML*) yloop 0
(*NOMINCAML done; *)

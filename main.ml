let limit = ref 1000

let syntax_option = ref false
let knormal_option = ref false
let alpha_option = ref false
let cse_option = ref false

let rec iter n e = 
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter (n - 1) e'

let lexbuf outchan l = 
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
          (Virtual.f
             (Closure.f
                (iter !limit
                   (let ast = Cse.f
                        (let ast = Alpha.f
                             (let ast =  KNormal.f
                                  (Typing.f 
                                     (let ast = Parser.exp Lexer.token l in
                                      if !syntax_option then print_endline ("syntax : \n" ^ (Syntax.show ast)) else () ; ast))
                              in if !knormal_option then print_endline ("knormal : \n" ^ (KNormal.show ast)) else () ; ast)
                         in if !alpha_option then print_endline ("alpha : \n" ^ (KNormal.show ast)) else (); ast)
                    in if !cse_option then print_endline ("cse : \n" ^ (KNormal.show ast)) else (); ast)
                )))))

let string s = lexbuf stdout (Lexing.from_string s)

let file f = 
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () =
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-syntax", Arg.Unit(fun () -> syntax_option := true), "dump ast of syntax");
     ("-knormal", Arg.Unit(fun () -> knormal_option := true), "dump ast of knormal");
     ("-alpha", Arg.Unit(fun () -> alpha_option := true), "dump ast of alpha");
     ("-cse", Arg.Unit(fun () -> cse_option := true), "dump ast of cse")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files


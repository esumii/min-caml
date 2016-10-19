let x = 1 in
let y = 2 in
let z = x + y in
let w = x + y in
let v = x + y in
let u = x + y in ()

(*
cse :                                                                                                                             
KNormal.Let (("x.1", Type.Int), (KNormal.Int 1),                                                                                  
  KNormal.Let (("y.2", Type.Int), (KNormal.Int 2),                                                                                
    KNormal.Let (("z.3", Type.Int), KNormal.Add ("x.1", "y.2"),                                                                   
      KNormal.Let (("w.4", Type.Int), (KNormal.Var "z.3"),                                                                        
        KNormal.Let (("v.5", Type.Int), (KNormal.Var "z.3"),                                                                      
          KNormal.Let (("u.6", Type.Int), (KNormal.Var "z.3"), KNormal.Unit))))))
*)
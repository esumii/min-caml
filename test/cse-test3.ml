let x = 1 in
let y = 2 in
let z = x + y in
let rec f w = let v = x + y in w + v in
let u = f 10 in ()  

(*
cse :                                                                                                                             
KNormal.Let (("x.2", Type.Int), (KNormal.Int 1),                                                                                  
  KNormal.Let (("y.3", Type.Int), (KNormal.Int 2),                                                                                
    KNormal.Let (("z.4", Type.Int), KNormal.Add ("x.2", "y.3"),                                                                   
      KNormal.LetRec (                                                                                                            
        { KNormal.name = ("f.5", Type.Fun ([Type.Int], Type.Int));                                                                
          args = [("w.6", Type.Int)];                                                                                             
          body = KNormal.Let (("v.9", Type.Int), (KNormal.Var "z.4"),                                                             
                   KNormal.Add ("w.6", "v.9")) },                                                                                 
        KNormal.Let (("u.7", Type.Int),                                                                                           
          KNormal.Let (("Ti1.8", Type.Int), (KNormal.Int 10),                                                                     
            KNormal.App ("f.5", ["Ti1.8"])), KNormal.Unit)))))
*)
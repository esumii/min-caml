
open KNormal

let find env e =
  try
    let a = Hashtbl.find env e
    in Var a
  with Not_found -> e

let mem env e =
  try
    let a = Hashtbl.find env e
    in Some a
  with Not_found -> None 

let rec g env = function
  | Unit -> Unit 
  | Int i -> Int i
  | Float i -> Float i
  | Neg x -> find env (Neg x)
  | Add (x, y) -> find env (Add (x, y))
  | Sub (x, y) -> find env (Sub (x, y))
  | Mul (x, y) -> find env (Mul (x, y))
  | Div (x, y) -> find env (Div (x, y))
  | FNeg x -> find env (FNeg x)
  | FAdd (x, y) -> find env (FAdd (x, y))
  | FSub (x, y) -> find env (FSub (x, y))
  | FMul (x, y) -> find env (FMul (x, y))
  | FDiv (x, y) -> find env (FDiv (x, y))
  | IfEq (x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | Let ((x, ty), e1, e2) -> (match mem env e1 with
      | Some y -> Let ((x, ty), Var y, g env e2)
      | None -> let e1_ = g env e1 in
        Hashtbl.add env e1 x; Let((x, ty), e1_, g env e2))
  | Var x -> Var x
  | LetRec ({ name = xt; args = yts; body = e1 }, ty) -> 
    LetRec ({ name = xt; args = yts; body = g env e1 }, ty)
  | App (f, xs) -> App (f, xs)
  | Tuple xs -> Tuple xs
  | LetTuple (xs, y, e) -> LetTuple (xs, y, g env e)
  | Get (x, y) -> Get (x,y)
  | Put (x, y, z) -> Put (x, y, z)
  | ExtArray x -> ExtArray x
  | ExtFunApp (x, ys) -> ExtFunApp (x, ys)

let f = g (Hashtbl.create 1000)
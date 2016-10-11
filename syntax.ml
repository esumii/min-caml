


type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of Id.pos * t
  | Neg of Id.pos * t
  | Add of Id.pos * t * t
  | Sub of Id.pos * t * t
  | Mul of Id.pos * t * t 
  | Div of Id.pos * t * t
  | FNeg of Id.pos * t
  | FAdd of Id.pos * t * t
  | FSub of Id.pos * t * t
  | FMul of Id.pos * t * t
  | FDiv of Id.pos * t * t
  | Eq of Id.pos * t * t
  | LE of Id.pos * t * t
  | If of Id.pos * t * t * t
  | Let of Id.pos * (Id.t * Type.t) * t * t
  | Var of Id.pos * Id.t
  | LetRec of Id.pos * fundef * t
  | App of Id.pos * t * (t list)
  | Tuple of (t list)
  | LetTuple of Id.pos * (Id.t * Type.t) list * t * t
  | Array of Id.pos * t * t
  | Get of Id.pos * t * t
  | Put of Id.pos * t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
[@@deriving show]
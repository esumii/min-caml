type opr = Id.t * Id.t * int
type opi = Id.t * int
type opj = int
type t = 
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  | Sw of opi
  | Lw of opi
  | Ori of opi
  | Andi of opi
  | Addi of opi
  | Add of opr
  | Slt of opr
  | Sub of opr
  | And of opr
  | Or of opr
  | Xor of opr
  | Sll of opr
  | Sra of opr
  | Clear
  | Comment of string
  | IfEq of Id.t * Id.t * t * t
  | IfNe of Id.t * Id.t * t * t
  | IfEqL of Id.t * Id.t * t * t
  | IfNeL of Id.t * Id.t * t * t
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t

type fundef = {
  name : Id.l;
  args : Id.t list;
  fargs : Id.t list;
  body : t;
  ret : Type.t;
}

type prog = Prog of (Id.l * float) list * fundef list * t
val seq : exp * t -> t
val regs : string array
val allregs : string list
val reg_cl : string
val reg_sw : string
val reg_jl : string
val reg_sp : string
val reg_hp : string
val is_reg : string -> bool
val remove_and_uniq : S.t -> S.elt list -> S.elt list
val fv_exp : exp -> S.elt list
val fv : t -> S.elt list
val concat : t -> Id.t * Type.t -> t -> t
val align : int -> int

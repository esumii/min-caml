(* MIniPS assembly with a few virtual instructions modified form Sparc *)

(* 各命令形式を表す型 *)
type opr = Id.t * Id.t * int
type opi = Id.t * int
type opj = int

type t = 
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp =
  (* 
  R : rs rt rd sa
  I : rs rt imi
  J : index
  *)
  | Nop
  | Sw of opi
  | Lw of opi
  | Ori of opi
  | Andi of opi
  | Addi of opi
  (* BNE, BEQ JAL J JR *)
  (* 比較, ジャンプ系はvirtual instructinoとして定義する *)
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
  (* virtual instructions *)
  | IfEq of Id.t * Id.t * t * t
  | IfNe of Id.t * Id.t * t * t
  | IfEqL of Id.t * Id.t * t * t
  | IfNeL of Id.t * Id.t * t * t
  (* closure address, integer arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t
type fundef =
  { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }

type prog = Prog of (Id.l * float) list * fundef list * t

(*let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)*)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 31 (fun i -> Printf.sprintf "%%r%d" i)

(*let fregs = Array.init 16 (fun i -> Printf.sprintf "%%f%d" (i * 2))*)
let allregs = Array.to_list regs
(*let allfregs = Array.to_list fregs*)
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
(*let reg_fsw = fregs.(Array.length fregs - 1)  temporary for swap *)
let reg_jl = "%r31" (* link register *)
let reg_sp = "%r30" (* stack pointer *)
let reg_hp = "%r29" (* heap pointer (caml2html: sparcasm_reghp) *)
let is_reg x = (x.[0] = '%')

(*let co_freg_table =
  let ht = Hashtbl.create 16 in
  for i = 0 to 15 do
    Hashtbl.add
      ht
      (Printf.sprintf "%%f%d" (i * 2))
      (Printf.sprintf "%%f%d" (i * 2 + 1))
  done;
  ht*)
(*let co_freg freg = Hashtbl.find co_freg_table freg  "companion" freg *)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
(*let fv_id_or_imm = function V(x) -> [x] | _ -> []*)

let rec fv_exp = function
  (* opr *)
  | Add(x, y, _) | Sub(x, y, _) | Slt(x, y, _) | And(x, y, _) | Or(x, y, _) | Xor(x, y, _) | Sll (x, y, _) | Sra (x, y, _) -> [x; y]
  (* opi *)
  | Sw (x, _) | Lw (x, _) | Ori (x, _) | Andi (x, _) | Addi (x, _) -> [x]
  (* virtual if *)
  | IfEq (x, y, e1, e2) | IfEqL (x, y, e1, e2) | IfNe (x, y, e1, e2) | IfNeL (x, y, e1, e2) -> remove_and_uniq S.empty (x :: y :: ((fv e1) @ (fv e2)))
  (* closure *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
    (* others *) 
  | Clear | Comment(_) | Restore(_) -> []
  | Save (x, y) -> [x] 
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
    fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)

let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)

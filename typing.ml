(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

(* fortmat for a type error message *)
(* type_err_fmt line column typename typename *)
(* let type_err_fmt = format_of_string "\x1b[1mline %d, column %d\x1b[0m: @.\x1b[1m\x1b[31mError\x1b[39m\x1b[0m: This expression has type %s but an expression was expected of type %s @." *)

let extenv = ref M.empty
(* for pretty printing (and type normalization) *)    
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
    Format.eprintf "uninstantiated type variable detected; assuming int@.";
    r := Some(Type.Int);
    Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
    let t' = deref_typ t in
    r := Some(t');
    t'
  | t -> t

let rec deref_id_typ (x, t) = (x, deref_typ t)

let rec deref_term = function
  | Not(p, e) -> Not(p, deref_term e)
  | Neg(p, e) -> Neg(p, deref_term e)
  | Add(p, e1, e2) -> Add(p, deref_term e1, deref_term e2)
  | Sub(p, e1, e2) -> Sub(p, deref_term e1, deref_term e2)
  | Mul(p, e1, e2) -> Mul(p, deref_term e1, deref_term e2)
  | Div(p, e1, e2) -> Div(p, deref_term e1, deref_term e2)
  | Eq(p, e1, e2) -> Eq(p, deref_term e1, deref_term e2)
  | LE(p, e1, e2) -> LE(p, deref_term e1, deref_term e2)
  | FNeg(p, e) -> FNeg(p, deref_term e)
  | FAdd(p, e1, e2) -> FAdd(p, deref_term e1, deref_term e2)
  | FSub(p, e1, e2) -> FSub(p, deref_term e1, deref_term e2)
  | FMul(p, e1, e2) -> FMul(p, deref_term e1, deref_term e2)
  | FDiv(p, e1, e2) -> FDiv(p, deref_term e1, deref_term e2)
  | If(p, e1, e2, e3) -> If(p, deref_term e1, deref_term e2, deref_term e3)
  | Let(p, xt, e1, e2) -> Let(p, deref_id_typ xt, deref_term e1, deref_term e2)
  | LetRec(p, { name = xt; args = yts; body = e1 }, e2) ->
    LetRec(p, { name = deref_id_typ xt;
                args = List.map deref_id_typ yts;
                body = deref_term e1 },
           deref_term e2)
  | App(p, e, es) -> App(p, deref_term e, List.map deref_term es)
  | Tuple(es) -> Tuple(List.map deref_term es)
  | LetTuple(p, xts, e1, e2) -> LetTuple(p, List.map deref_id_typ xts, deref_term e1, deref_term e2)
  | Array(p, e1, e2) -> Array(p, deref_term e1, deref_term e2)
  | Get(p, e1, e2) -> Get(p, deref_term e1, deref_term e2)
  | Put(p, e1, e2, e3) -> Put(p, deref_term e1, deref_term e2, deref_term e3)
  | e -> e

let rec occur r1 = function (* occur check (caml2html: typing_occur) *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = (* 型が合うように、型変数への代入をする (caml2html: typing_unify) *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
    unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 (caml2html: typing_undef) *)
    if occur r1 t2 then raise (Unify(t1, t2));
    r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
    if occur r2 t1 then raise (Unify(t1, t2));
    r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let err_handler p x y =
  try
    unify x y
  with
    Unify(_, _) as ex ->
    (Format.eprintf
       "\x1b[1mline %d, column %d\x1b[0m: @.\x1b[1m\x1b[31mError\x1b[39m\x1b[0m: This expression has type %s but an expression was expected of type %s @."
       (fst p)
       (snd p)
       (*
       (Type.show (deref_typ y))
       (Type.show (deref_typ x)))*)

       (Type.show y) (Type.show x))
    ;
    raise ex

let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  try
    match e with
    | Unit -> Type.Unit
    | Bool(_) -> Type.Bool
    | Int(_) -> Type.Int
    | Float(_) -> Type.Float
    | Not(p, e) ->
      err_handler p Type.Bool (g env e);
      Type.Bool
    | Neg(p, e) ->
      err_handler p Type.Int (g env e);
      Type.Int
    | Add(p, e1, e2) | Sub(p, e1, e2) | Mul(p, e1, e2) | Div(p, e1, e2) -> (* 足し算（と引き算）の型推論 (caml2html: typing_add) *)
      err_handler p Type.Int (g env e1);
      err_handler p Type.Int (g env e2);
      Type.Int
    | FNeg(p, e) ->
      err_handler p Type.Float (g env e);
      Type.Float
    | FAdd(p, e1, e2) | FSub(p, e1, e2) | FMul(p, e1, e2) | FDiv(p, e1, e2) ->
      err_handler p Type.Float (g env e1);
      err_handler p Type.Float (g env e2);
      Type.Float
    | Eq(p, e1, e2) | LE(p, e1, e2) ->
      err_handler p (g env e1) (g env e2);
      Type.Bool
    | If(p, e1, e2, e3) ->
      err_handler p Type.Bool (g env e1);
      let t2 = g env e2 in
      let t3 = g env e3 in
      err_handler p t2 t3;
      t2
    | Let(p, (x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
      err_handler p t (g env e1);
      g (M.add x t env) e2
    (* | Var(p, x) when (M.mem x env && x = "d_vec") -> let line = fst p in if line = 2302 then print_endline ("----"^(string_of_int line)^(Type.show (deref_typ (M.find x env)))); M.find x env *)
    | Var(_, x) when M.mem x env -> M.find x env (* 変数の型推論 (caml2html: typing_var) *)
    | Var(_, x) when M.mem x !extenv -> M.find x !extenv
    | Var(_, x) -> (* �����ѿ��η����� (caml2html: typing_extvar) *)
      Format.eprintf "free variable %s assumed as external@." x;
      let t = Type.gentyp () in
      extenv := M.add x t !extenv;
      t
    | LetRec(p, { name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
      let env = M.add x t env in
      err_handler p t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
      g env e2
    | App(p ,e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
      let t = Type.gentyp () in
      err_handler p (g env e) (Type.Fun(List.map (g env) es, t));
      t 
    | Tuple(es) -> Type.Tuple(List.map (g env) es)
    | LetTuple(p, xts, e1, e2) ->
      err_handler p (Type.Tuple(List.map snd xts)) (g env e1);
      g (M.add_list xts env) e2
    | Array(p, e1, e2) -> (* must be a primitive for "polymorphic" typing *)
      err_handler p (g env e1) Type.Int;
      Type.Array(g env e2)
    | Get(p, e1, e2) ->
      let t = Type.gentyp () in
      err_handler p (Type.Array(t)) (g env e1);
      err_handler p Type.Int (g env e2);
      t
    | Put(p, e1, e2, e3) ->
      let t = g env e3 in
      err_handler p (Type.Array(t)) (g env e1);
      err_handler p Type.Int (g env e2);
      Type.Unit
  with Unify(t1, t2) -> raise (Error(deref_term e, deref_typ t1, deref_typ t2))

let f e =
  extenv := M.empty;
(*
  (match deref_typ (g M.empty e) with
  | Type.Unit -> ()
  | _ -> Format.eprintf "warning: final result does not have type unit@.");
*)
  (try unify Type.Unit (g M.empty e)
   with Unify _ -> failwith "top level does not have type unit");
  extenv := M.map deref_typ !extenv;
  deref_term e


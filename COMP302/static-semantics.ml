type op = Minus | Plus | Times | Lt | Eq

type name = string

type exp =
  | N of int
  | B of bool
  | Op of exp * op * exp
  | If of exp (*condition*) * exp (*then-branch*) * exp (*else-branch*)
  | Let of name * exp * exp

                 (*
  e !! true      e1 !! v            e1 !! v1     e2 !! v2
--------------------------         ------------------------
if e then e1 else e2 !! v            e1 op e2 !! do (v1 op v2)
*)

exception RuntimeError of string

let do_op (op : op) (v1 : exp) (v2 : exp) = match op, v1, v2 with
  | Plus, N n1, N n2 -> N (n1 + n2)
  | Times, N n1, N n2 -> N (n1 * n2)
  | Minus, N n1, N n2 -> N (n1 - n2)
  | Eq, N n1, N n2 -> B (n1 = n2)
  | Eq, B b1, B b2 -> B (b1 = b2)
  | Lt, N n1, N n2 -> B (n1 < n2)
  | _ -> raise (RuntimeError "bad operation arguments")

  (* subst (e', x) e = [e'/x]e performs a capture-avoiding substitution of e' for x in e. *)
let rec subst (e', x : exp * name) (e : exp) : exp = failwith "exercise"

let rec eval (e : exp) : exp = match e with
  | N n -> N n
  | B b -> B b
  | Op (e1, op, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      do_op op v1 v2
  | If (e, e1, e2) ->
      begin match eval e with
        | B b -> if b then eval e1 else eval e2
              (* | NV n -> if n <> 0 then eval e1 else eval e2 *)
        | _ -> raise (RuntimeError "if condition must be boolean")
      end
  | Let (x, e1, e2) ->
      let v = eval e1 in
      eval (subst (v, x) e2)

(* if 2 + 2 = 5 then true * false else 17 *)
let ex = If (
    Op (Op (N 2, Plus, N 3), Eq, N 5) (* 2 + 2 = 5 *),
    Op (B true, Times, B false), (* true * false *)
    N 17 (* 17 *)
  )

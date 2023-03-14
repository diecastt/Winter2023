(*
   March 13, 2023 Lecture
   
   Grammar :
   
   Operations op ::= - | + | * | < | =
   Expressions e ::= n | e1 op e2 | if e then e1 then e2 | true | false
   
*)

type op = Minus | Plus | Times | Lt | Eq
type exp  = 
  | N of int 
  | B of bool 
  | Op of exp * op * exp 
  | If of exp (*condition*) * exp (* then-branch *) * exp (* else-branch *)
            
type value = NV of int | BV of bool
exception RuntimeError of string
               
let do_op (op : op) (v1 : value) (v2 : value) = match op, v1, v2 with
  | Plus, NV n1, NV n2 -> NV (n1 + n2)
  | Times, NV n1, NV n2 -> NV (n1 * n2) 
  | Times, BV b1, BV b2 -> BV (b1 && b2)
  | Minus, NV n1, NV n2 -> NV (n1 - n2) 
  | Eq, NV n1, NV n2 -> BV (n1 = n2)
  | Eq, BV b1, BV b2 -> BV (b1 = b2)
  | Lt, NV n1, NV n2 -> BV (n1 < n2) 
  | _ -> raise (RuntimeError "Bad Operation Arguments")
                            
let rec eval (e : exp) : value = match e with
  | N n -> NV n
  | B b -> BV b
  | Op (e1, op, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      do_op op v1 v2
  | If (e, e1, e2) ->
      match eval e with 
      | BV b -> if b then eval e1 else eval e2                                                
      | _ -> raise (RuntimeError "if condition must be boolean")
      (*| NV n -> if n <> 0 then eval n1 else eval e2*)

let ex = If (
    Op (Op (N 2, Plus, N 2), Eq, N 5), 
    Op (B true, Times, B false),
    N 17 
  ) 
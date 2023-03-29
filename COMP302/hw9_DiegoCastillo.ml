let ctx1 = [("x", Int); ("y", Int)]
let ctx2 = [("x", Bool); ("y", Bool)]
let ctx3 = [("a", Int)]


(* Write some SUCCESS test cases for `infer` *)
let infer_tests : ((ctx * exp) * tp) list = [
  ((ctx1, Primop (Equals, [Var "x"; Var "y"])), Int)

]

(* Q1: `infer_op` - Type Inference in Primitive Operations *)

(* [infer_op] returns the type of a primitive operation *)
let infer_op (op : primop) (ts : tp list) : tp = 
  let len = List.length ts in 
  match (op, ts) with 
  | Negate, _ when len <> 1 -> raise ArityMismatch 
  | Negate, [tp] -> (match tp with 
      | Int -> Int
      | _ -> raise TypeMismatch)
  | _, _ when len <> 2 -> raise ArityMismatch
  | Equals, [tp1; tp2] -> (match (tp1, tp2) with
      | Arrow _, _ -> raise TypeMismatch
      | _, Arrow _ -> raise TypeMismatch
      | x, y when x <> y -> raise TypeMismatch
      | _, _ -> Bool)
  | LessThan, [tp1; tp2] -> (match (tp1, tp2) with
      | Int, Int -> Bool
      | _, _ -> raise TypeMismatch)
  | _, [tp1; tp2] -> (match (tp1, tp2) with
      | Int, Int -> Int
      | _, _ -> raise TypeMismatch)
             

(* Q2: `infer` - General Type Inference *)

(* [infer] returns the type of an expression in a context *)
let rec infer (ctx : ctx) (e : exp) : tp =
  match e with
  | I _ -> Int
  | B _ -> Bool
  | Var x -> if List.assoc_opt ctx = None then raise FreeVariable else List.assoc_opt ctx
  | Primop (op, es) -> infer_op op (List.map (infer ctx) es)
  | If (cond, e1, e2) -> (match ((infer ctx cond), (infer ctx e1), (infer ctx e2)) with
      | Bool, x, y -> x
      | _, _, _ -> raise TypeMismatch)
  | Let (x, e1, e2) -> infer ((x, infer ctx e1) :: ctx) e2
  | Fn (xs, e') -> infer (ctx @ xs) e'
  | Apply (e', args) -> raise NotImplemented
  | Rec (f, t, e') -> raise NotImplemented

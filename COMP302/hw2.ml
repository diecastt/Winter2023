(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = []

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_nat_of_int (n : int) : nat = 
  let rec helper (n : int) (acc : nat) = 
    if n = 0 then acc
    else
      helper (n - 1) (S (acc))
  in
  helper n Z
  
(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = []

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_int_of_nat (n : nat) : int = 
  let rec helper (n : nat) (acc: int) = 
    match n with
    | Z -> acc
    | S n' -> helper n' (acc + 1) 
  in
  helper n 0
    

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = []

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat = 
  match m with
  | Z -> n
  | S m' -> q1c_add (S (n)) (m')

(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times ( 
    Const (-1.0),
    e
  )

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus(
    e1, 
    q2a_neg e2
  )
    

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp = 
  let rec helper e1 p acc = 
    match p with 
    | Z -> acc
    | S n' -> helper e1 n' (Times(e1, acc))
  in
  helper e1 p (Const 1.)
    
  
(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = []

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with 
  | Plus (c1, c2) -> eval a c1 +. eval a c2
  | Times (c1, c2) -> eval a c1 *. eval a c2
  | Div (c1, c2) -> eval a c1 /. eval a c2
  | Var -> eval a (Const a)
  | Const c -> c 
                                
                           

(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = []

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = raise Not_implemented

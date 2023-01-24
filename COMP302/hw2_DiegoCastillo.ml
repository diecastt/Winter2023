(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [ 
  (0, (Z));
  (1, (S Z));
  (4, (S (S (S ( S Z )))) );
  (5, (S( S (S (S ( S Z )))))) 
]

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_nat_of_int (n : int) : nat = 
  let rec helper (n : int) (acc : nat) = 
    if n = 0 then acc
    else
      helper (n - 1) (S (acc))
  in
  helper n Z
  
(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [
  ((Z), 0);
  ((S Z), 1); 
  ((S (S (S ( S Z )))), 4);
  ( (S( S (S (S ( S Z ))))), 5) 
]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_int_of_nat (n : nat) : int = 
  let rec helper (n : nat) (acc: int) = 
    match n with
    | Z -> acc
    | S n' -> helper n' (acc + 1) 
  in
  helper n 0
    

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((Z, Z), Z);
  ((S Z, Z), S Z);
  (( (S (S (S ( S Z )))), S Z), S (S (S (S ( S Z )))) )
]


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

(* Compiled the equation given as an example to use as test case for Q3 and Q4 *)
let eq1 = Plus (
    Plus (
      Times (
        Const 2.0,
        Var
      ),
      Times (
        Const (-1.0),
        Div (Var, Const 3.0)
      )
    ),
    Const 10.0
  )

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  ((3.0, eq1), 15.);
  ((6.0, eq1), 20.);
  ((2.0, Plus(Var, Const (-1.0))), 1.0);
]

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
let diff_tests : (exp * exp) list = [
  (eq1, (Plus
           (Plus (Plus (Times (Const 0., Var), Times (Const 2., Const 1.)),
                  Plus (Times (Const 0., Div (Var, Const 3.)),
                        Times (Const (-1.),
                               Div
                                 (Plus (Times (Const 1., Const 3.),
                                        Times (Const (-1.), Times (Var, Const 0.))),
                                  Times (Const 3., Const 3.))))),
            Const 0.)))
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = match e with
  | Plus (e1, e2) -> Plus( diff e1, diff e2)
  | Times (e1, e2) -> Plus (Times (diff e1, e2), Times (e1, diff e2))
  | Const c -> Const 0.
  | Var -> Const 1.
  | Div (e1, e2) -> Div(
      q2b_minus (Times(diff e1, e2)) (Times(e1, diff e2)), 
      Times(e2, e2) 
        
    )
      
    

(* Formulas for testing *)
let f1 = parse_formula "(a | ~b) & c"
let f2 = parse_formula "(a | ~b) & ~(c | d)"
let f3 = parse_formula "a | a & a2"
let f4 = parse_formula "~~a"
(** Question 1 *)

(* TODO: Add test cases. *)
let collect_variables_tests : (formula * Variable_set.t) list = 
  [
    (f1, Variable_set.singleton "a"
         |> Variable_set.add "b"
         |> Variable_set.add "c");
    (f2, Variable_set.singleton "a"
         |> Variable_set.add "b"
         |> Variable_set.add "c"
         |> Variable_set.add "d");
    (f4, Variable_set.singleton "a");
    (f3, Variable_set.singleton "a"
         |> Variable_set.add "a2");
  ]

(* TODO: Implement the function. *)
let rec collect_variables (formula : formula) : Variable_set.t =
  match formula with
  | Variable x -> Variable_set.singleton x
  | Negation x -> collect_variables x 
  | Disjunction (l, r) -> Variable_set.union (collect_variables l) (collect_variables r)
  | Conjunction (l, r) -> Variable_set.union (collect_variables l) (collect_variables r)
  
  

(** Question 2 *)

(* TODO: Add test cases. *)
let eval_success_tests : ((truth_assignment * formula) * bool) list = []

(* TODO: Add test cases. *)
let eval_failure_tests : ((truth_assignment * formula) * exn) list = []

(* TODO: Implement the function. *)
let rec eval (state : truth_assignment) (formula : formula) : bool =
  let table_lookup var (table : truth_assignment) =
    match (Variable_map.find_opt var table) with
    | None -> raise (Unassigned_variable var)
    | Some x -> x
  in
  match formula with 
  | Variable x -> table_lookup x state
  | Negation x -> not (eval state x)
  | Disjunction (l, r) -> let fx = eval state l in let gy = eval state r in fx || gy
  | Conjunction (l, r) -> let fx = eval state l in let gy = eval state r in fx && gy
(** Question 3 *)

(* TODO: Add test cases. *)
let find_satisfying_assignment_tests : (formula * truth_assignment option) list = []

(* TODO: Implement the function. *)
let find_satisfying_assignment (formula : formula) : truth_assignment =
  raise Not_implemented


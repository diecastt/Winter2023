let free_test1 = Fn ([("x", Int); ("y", Int)], 
                     Primop (Plus,
                             [Primop (Times, [Var "x"; Var "c"]);
                              Primop (Times, [Var "z"; Var "y"])]))
let free_test2 = 
  Let ("f",
       Fn ([("x", Int)],
           Fn ([("y", Int)],
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "o"]);
                        Primop (Times, [Var "d"; Var "b"])]))),
       Apply (Apply (Var "f", [I 3]),
              [I 4]))
    
let free_test3 = Rec ("f", Arrow ([Int], Int), Apply (Var "f", [Var "x"]))
    
let free_test4 = 
  Apply ((Primop (Times, [I 4])), [I 2; I 4; I 5])

(**To DO: Write a good set of tests for free_variables **)
let free_variables_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Let, Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), []);
  (free_test1, ["c" ; "z"]);
  (free_test2, ["o"; "d"; "b"]);
  (free_test3, ["x"]);
  (free_test4, [])
  
]

(* TODO: Implement the missing cases of free_variables. *)
let rec free_variables : exp -> name list =
  (* Taking unions of lists.
     If the lists are in fact sets (all elements are unique),
     then the result will also be a set.
  *)
  let union l1 l2 = delete l2 l1 @ l2 in
  let union_fvs es = 
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) -> union_fvs [e; e1; e2]
  | Primop (_, args) -> union_fvs args
  | Fn (xs, e) -> 
      let (vars, _) = List.split xs in
      List.filter (fun y -> not (List.mem y vars)) (union_fvs [e])
  | Rec (x, _, e) -> List.filter (fun y -> x <> y) (union_fvs [e])
  | Let (x, e1, e2) -> union_fvs [e1] @ List.filter (fun y -> y <> x) (union_fvs [e2]) 
  | Apply (e, es) -> 
      union_fvs [e] @ union_fvs es


(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)

let subs_test1 = (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
                   Rec ("y", Int , Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
                  Rec ("y", Int , Primop (Plus, [Var "y"; I 1])))
                 
let subs_test2 = (((Var "z", "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
                   Apply (Primop (Times, [I 2]), [Primop (Plus, [Var "y"; Var "x"])])),
   (* let y = 2 in y + 1 *)
                  Apply (Primop (Times, [I 2]), [Primop (Plus, [Var "y"; Var "z"])]))
                 
let subs_test3 = (((Var "z", "y"), (* [1/x] *)
    (* let y = 2 in y + x *)
                   ex1),
   (* let y = 2 in y + 1 *)
                  Fn ([("x", Int); ("z", Int)],
                      Primop (Plus,
                              [Primop (Times, [Var "x"; Var "x"]);
                               Primop (Times, [Var "z"; Var "z"])])))
                 
let subs_test4 = (((I 1, "y"), (* [1/x] *)
    (* let y = 2 in y + x *)
                   Rec ("y", Int , Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
                  Rec ("y", Int , Primop (Plus, [Var "y"; Var "x"])))
                 
let subs_test5 = (((I 1, "y"), (* [1/x] *)
    (* let y = 2 in y + x *)
                   Fn ([("x", Int)], Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
                  Fn ([("x", Int)], Primop (Plus, [I 1; Var "x"]))) 

let subs_test6 =  (((Var "f", "x"), (Rec ("f", Int, Var "x"))), (Rec ("f1", Int, Var "f"))) 
                  
let subs_test7 =  (((Var "y","x"), (Fn ([("y", Int)], (Var "x")))), (Fn ([("y1", Int)], (Var "y"))))
                    
let subs_test8 =  (((I 1, "x"), (Apply (Var "x", [Var "y"; Var "x"]))), (Apply (I 1, [Var "y"; I 1])))
                 
let subst_tests : (((exp * name) * exp) * exp) list = [
  subs_test1;
  subs_test2;
  subs_test3;
  subs_test4;
  subs_test5;
  subs_test6;
  subs_test7;
  subs_test8
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if y = x then
        Rec (y, t, e)
      else
        let (y, e) = 
          if List.mem y (free_variables e') then
            rename y e
          else
            (y, e)
        in
        Rec (y, t, subst s e)

  | Fn (xs, e) -> 
      let (vars, types) = List.split xs in
      let is_present x = List.exists (fun y -> y = x) (free_variables e') in
      let hasFreeVariable = List.exists is_present vars in

      if List.exists (fun y -> y = x) vars then
        Fn (xs, e)
      else
        let (xs, e) =
          if hasFreeVariable then
            let (new_vars, e) = rename_all vars e in
            ((List.combine new_vars types), e) 
          else
            (xs, e)
        in
        Fn (xs, subst s e) 
          
  | Apply (e, es) -> Apply (subst s e, List.map (subst s) es)

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs

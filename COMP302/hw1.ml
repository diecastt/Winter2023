(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [ (3,4);(3,4) 
    (* Your test cases go here *)
                     ]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*) 
    
let distance (x1, y1) (x2, y2) = 
  let abs a b = if (a - b) < 0 then -(a - b) else (a - b)
  in
  abs x1 x2 + abs y1 y2
    
(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1); ((10, 4), 210); ((10, 4), 210); ((5, 3), 10); ((16, 4), 1820)

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial n k =
  if n = k then 1 else  
    let rec factorial i = 
      if i <= 1 then 1 else (factorial (i - 1) * i)
    in
    (factorial n) / ((factorial k) *  factorial (n-k)) 
                    
let rec factorial i = 
  if i <= 1 then 1 else (factorial (i - 1) * i)
                        

(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
]

(* TODO: Implement a tail-recursive helper lucas_helper. *)
let rec lucas_helper acc1 acc2 =
  


(* TODO: Implement lucas that calls the previous function. *)
  let lucas n =
    raise NotImplemented

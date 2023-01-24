(*
  January 23, 2023 Lecture
*)

(* List , Reverse a list example*)

let rec rev (l : 'a list) : 'a list = 
  match l with
  | [] -> []
  | x :: xs -> rev xs @ [x] (* x :: []*) 

(* Tracing 
   
  rev [1;2;3]
   = rev (1 :: (2 :: (3:: [])))
   = rev [2;3] @ [1]
   = (rev [3] @ [2]) @ [1]
   = ((rev [] @ [3]) @ [2]) @ [3]
   = (([] @ [3]) @ [2]) @ [3]
   = (([3] @ [2]) @ [1])
   = [3;2] @ [1]
   = [3;2;1]
   
*)

let rec tail_rev (l : 'a list) (acc : 'a list) : 'a list = 
  match l with
  | [] -> acc
  | x :: xs -> tail_rev xs (x :: acc)

let rec filter (p : 'a -> bool) (l: 'a list) : 'a list = 
  match l with
  | [] -> []
  | x :: xs ->
      if p x then
        let ys = filter p xs in
        x :: ys
      else
        filter p xs

let rec map (f: 'a -> bool) (l: 'a list) : 'b list = 
  match l with
  | [] -> []
  | x :: xs ->
      f x :: map f xs
        
let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (e : 'b) : 'b = 
  match l with
  | [] -> e
  | x :: xs -> f x (fold_right f xs e)
  (* 
     fold_right f [a1;a2;...;aN] e 
     = f aN-1 (f aN e)
  
  *)
  
(* 
   Exercises:
   - Write filter in terms of fold_right
   - Write map in terms of fold_right
*)

(*
      type 'a myoption = 
      | Some of 'a
      | None
*)

let rec find (p : 'a -> bool) (l : 'a list) : 'a option = 
  match l with
  | [] -> ???
  | x :: xs -> ?
    
let rec alt (l : 'a option list): 'a option = failwith "exercise"
  
let rec all (l: 'a option list) : 'a list option
    
    

  
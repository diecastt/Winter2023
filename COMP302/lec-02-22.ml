(*
   Feb 22 Lecture, Induction

*)

(*

THEOREM (Map distributes over append.)
   For any l1, l2: 'a list and any f: 'a -> 'b, we have:
   app (map f l1) (map f l2) = map f (app l1 l2)
   
   PROOF.
   
   CASE. l1 = []
   
   NTS: app (map f []) (map f l2) = map f (app [] l2)
   
   LHS = app (map f []) (map f l2)
       = app [] (map f l2)            -- by map
       = mao f l2                     -- by app
       
   RHS = map f (app [] l2)      
       = map f l2
       = LHS
       
  CASE. l1 = x :: xs.
  
  NTS: app (map f (x :: xs)) (map f l2) = map f (app (x :: xs) l2)
  IH:  app (map f xs) (map f l2) = map f (app xs l2)

  LHS = app (map f (x :: xs)) (map f l2)
      = app (f x  :: map f xs) (map f l2)   -- by map
      = f x :: app (map f xs) (map f l2)    -- by app
      = f x :: map f (app xs l2)            -- by IH

  RHS = map f (app (x :: xs) l2)
      = map f (x :: app xs l2)
      = f x :: map f (app xs l2)
      = LHS
    QED.
*)
(*
THEOREM. (sum and sum_tr are equivalent. Take 2.Need to generalize problem!)

For any l: int list and any acc: int, we have:
  Acc + sum l = sum_tr l acc

PROOF. By induction on l

Case l = []
  NTS: acc + sum [] = sum_tr [] acc
  
  LHS = acc + sum []
      = acc + 0      -- by def of sum
      = acc          -- by common sense
   
  RHS = sum_tr [] acc
      = acc     - by def of sum_tr
      = LHS
   
 Case l = x :: xs
   NTS:  acc + sum (x :: xs) = sum_tr (x :: xs) acc
   IH: for any acc: int, we have: acc + sum xs = sum_tr xs acc

   LHS = acc + sum (x :: xs)
       = acc + (x + sum xs)
   
   RHS = sum_tr (x :: xs) acc
       = sum_tr xs (acc +x)
       = (acc + x) + sum xs    -- by IH with acc := acc + x
           -- specialized IH: (acc + x) + sum xs = sum_tr xs (acc + x)
       = LHS      -- by common sense
   
*)

let rec map_tr f l return = match l with
  | [] -> return []
  | x :: xs -> map_tr f xs (fun ys -> return (f x :: ys))
(*
   Exercise: THEOREM. (map and map_tr are equivalent)

 For any f : 'a -> 'b and any l: 'a list, we have:
  map f l = map_tr f l (fun x -> x)

 We need to generalize this! 
 
 For any f : 'a -> 'b, any l: 'a list and any return : 'b list -> 'r, we have:
    return (map f l) = map_tr f l return
   
   *)


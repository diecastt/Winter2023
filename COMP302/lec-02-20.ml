let rec map f l = match l with
  | [] -> []
  | x :: xs -> f x :: map f xs    
                 
let rec app l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> x :: app xs l2
                 
                 (* 1. app (x :: xs) l2 = x :: app xs l2 *) 
                 
let rec len l = match l with
  | [] -> 0
  | x :: xs -> 1 + len xs 
                           
       (*
Equations for len:
1. len [] = 0
2. len (x :: xs) = 1 + len xs      for any x : 'a, xs : 'a list
*) 

                 (*
THEOREM (append adds length).
  For any lists l1, l2 : 'a list, we have:
    len l1 + len l2 = len (app l1 l2)

PROOF. By induction on l1.

CASE. l1 = [].
      WTS: len [] + len l2 = len (app [] l2)

      LHS = len [] + len l2
          = 0 + len l2      -- by defn of len
          = len l2          -- by common sense
          
      RHS = len (app [] l2)
          = len l2           -- by defn of app
          = LHS
          
CASE. l1 = x :: xs. 
      WTS: len (x :: xs) + len l2 = len (app (x :: xs) l2)
      IH: len xs + len l2 = len (app xs l2)

      LHS = len (x :: xs) + len l2
          = 1 + len xs + len l2      -- by defn len
          = 1 + len (app xs l2)      -- by IH
          = len (x :: app xs l2)     -- by len, backwards
          = len (app (x :: xs) l2)   -- by app, backwards
          = RHS
          
QED.
*)
                 
                 (* 
THEOREM (map preserves length).
  For any list l : 'a list, and any f : 'a -> 'b, we have:
    len l = len (map f l)

PROOF. By induction on l.      -- match l with

CASE. l = [].
      WTS: len [] = len (map f [])
      LHS = len [] = 0    -- by defn of len
      RHS = len (map f [])
          = len []       -- by defn of map
          = 0            -- by defn of len
          
CASE. l = x :: xs.
      WTS: len (x :: xs) = len (map f (x :: xs))
      IH: len xs = len (map f xs)

      LHS = len (x :: xs)
          = 1 + len xs           -- by defn of len
          = 1 + len (map f xs)   -- by IH

      approach one, using backwards definitions:
          = len (f x :: map f xs) -- by len, backwards
          = len (map f (x :: xs)) -- by map, backwards
          = RHS
          
      approach two, pivoting to the RHS:
      RHS = len (map f (x :: xs))
          = len (f x :: map f xs) -- by defn of map
          = 1 + len (map f xs)    -- by defn of len
          = LHS
          
QED.

*) 

(* EXERCISE: app (map f l1) (map f l2) = map f (app l1 l2) *)
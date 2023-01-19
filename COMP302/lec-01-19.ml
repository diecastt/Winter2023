(*
Jan 18, 2023 Lecture

Lists, Polymorphic types, high-order functions
   
*)

type nat = Z | S of nat
             
let zero = Z
let one = S Z
let five = S (S (S (S (S Z))))
let six = S five
    
let rec nat2int (n : nat) : int = match n with
  | Z -> 0
  | S n' ->
      1 + nat2int n'
(*
Trace:
1. nat2int (S (S (S Z)))
2. 1 + nat2int (S (S Z))
3. 1 + (1 + nat2int (S Z))
4. 1 + (1 + (1 + nat2int Z))
5. 1 + (1 + (1 + 0)))
6. 3
   
   *)
(* Cards Example *)
type color = Red | Green | Yellow | Blue
type value = Numeric of int | Reverse | Skip | Plus2
type card = color * value
            
            
type hand = Empty | Card of card * hand
                            
let h1 = Card ( (Blue, Reverse),
                Card ( (Green, Skip), 
                       Card ( (Yellow, Numeric 8), 
                              Card ( (Yellow, Skip), Empty))))
  
let rec length (h : hand) (acc: int) : int = match h with
  | Empty -> acc
  | Card (c, h') ->
      length h' (acc + 1) 
  
let id x = x
  
let const k x = k
  
type 'a mylist = Nil | Cons of 'a * ('a mylist)
                                 
let rec length (l : 'a mylist) (acc: int) : int = match l with
  | Nil -> acc
  | Cons (x, xs) ->
      length xs (acc + 1) 
        
let rec filter (p : 'a -> bool) (l : 'a mylist) : 'a mylist = 
  match l with
  | Nil -> Nil
  |Cons (x, xs) ->
      if p x then
        let ys = filter p xs in 
        Cons (x, xs)
      else
        filter p xs
              
type name = string
  
type height = int
  
type person = name * height
              
let jake : person = ("Jake", 182)
  
type color = Red | Green | Yellow | Blue
   
type value = Numeric of int | Reverse | Skip | Plus2          
   
type card = color * values
            
let can_follow (c1: card) (c2: card) = 
  let (col1, val1) = c1 in
  let (col2, val2) = c2 in
  col1 = col2 || val1 = val2
                 
let can_follow c1 c2 = 
  match c1 with 
  | (col1, val1) -> 
      match c2 with
      | (col2, val2) -> col1 = col2 || val1 = val2
                          
let can_follow c1 c2 = 
  match (c1, c2) with (* card * card == (color * value) * (color * value) *)
  | ((col1, val1), (col2, val2)) -> col1 = col2 || val1 = val2

let is_favourite_uno_val v = match v with
  | Numeric 4 -> true
  | _ -> false
  
type rps = Rock | Paper | Scissors
           
type outcome = Win | Draw | Loss
               
               
let play (p1: rps) (p2: rps) : outcome = 
  match (p1, p2) with
  | _ when p1 = p2 -> Draw
  | Rock, Scissors -> Win
  | Paper, Rock -> Win
  | Scissors, Paper -> Win
  | _ -> Loss
  
type nat = Z | S of nat
             
let nat2int (n:int) : int match in with
| Z -> 0
| S n -> 
  let k = nat2int n in 
  k + 1
  
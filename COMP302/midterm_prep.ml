let last (l : 'a list) : 'a option = 
  let rec helper list acc = match list with
    | [] -> acc
    | x :: xs -> helper xs (Some x)
  in 
  helper l None 
    
let rec last_two l = match l with
  | [] -> None
  | [x] -> None
  | [x; y] -> Some (x,y) 
  | x :: xs -> last_two xs 
                 
let rec rev (l : 'a list) (acc : 'a list) = match l with
  | [] -> acc
  | x :: xs -> rev xs (x :: acc)
                 
let is_palindrome (l : 'a list) = 
  if l = rev l [] then true else false 
    
let drop (l : 'a list) n = 
  let rec helper list target counter = match list with
    | [] -> []
    | x :: xs ->
        if not (target = counter) then
          x :: (helper xs target (counter + 1))
        else
          helper xs target (counter + 1)
  in
  helper l n 0
      
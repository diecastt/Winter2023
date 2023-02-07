(*
   Feb 6, 2023 Lecture
   
   *)
   
type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree
              
let rec find (p : 'a -> bool) (t: 'a tree) : 'a option = 
  match t with
  | Empty -> None
  | Node (_, x, _) when p x -> Some x
  | Node (l, x, r) -> 
      match (find p l) with 
      | None -> find p r
      | Some x -> Some x 
                    
let rec find_tr (p : 'a -> bool) (t: 'a tree) (return : 'a -> 'r)
    (backtrack : unit -> 'r) : 'r = 
  match t with 
  | Empty -> backtrack () 
  | Node (_, x, _) when p x -> return x
  | Node (l, _, r) ->
      find_tr p l (fun x -> return x) (fun () -> find_tr p r return backtrack)
                    
let tree : 'a tree = Node ( Node (Empty, 11, Empty), 32, Node (Empty, 4, Empty))
    
type coin = int
let canada_coins : coin list = [200; 100; 25; 10; 5; 1]

exception Change (* Defines a new exception named Change *)
                 (* Exception belong to the type exn *)
                 (* an 'exception' declaration adds a new construction to the type exn *)

let rec change (coins : coin list) (amt : int) : coin list = 
  match coins with
  | _ when amt = 0 -> []
  | [] -> raise Change
  | c :: cs when c > amt -> change cs amt
  | c :: cs -> 
      try
        c :: change (c :: cs) (amt - c)
      with
      | Change -> change cs amt
                    
let rec change (coins : coin list) (amt : int) 
    (return : coin list -> 'r) (backtrack : unit -> 'r) : coin list = 
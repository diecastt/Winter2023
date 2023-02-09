(*
   Feb 8, 2023 Lecture
   Modules, Object Oriented Programming
   
*)

type person = string * int
              
let people = [
  ("jake", 180);
  ("jake", 180);
  ("maggie", 50);
  ("maggie", 50); 
  ("maggie", 50); 
  ("winkel", 170); 
]

module PersonOrder : Set.OrderedType with type t = person = struct
  type t = person
    
  let compare ((name1, height1) : person) ((name2, height2) : person) = 
    let x = compare name1 name2 in
    if x = 0 then compare height1 height2 else x
  
end

module PersonSet = Set.Make (PersonOrder)
    
let rec dedup (l : person list) (s : PersonSet.t) = match l with
  | [] -> PersonSet.elements s
  | x :: xs -> dedup xs (PersonSet.add x s)
  
let make_tick () = 
  let count = ref 0 in
  ((fun () -> count := !count + 1; !count), 
   (fun () -> count := 0))
      
let (tick, reset) = make_tick()
    
let (tick', reset') = make_tick ()
    

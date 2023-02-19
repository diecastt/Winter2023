(*
   Feb 13, 2023 Lecture: Lazy Programming
   *)
  
  (*Re-writing make_tick in 3 different ways
  1. syntax
  2. suspended computations
  3. streams *)


(* record syntax *)
(* records are tuples but we identify the fields by name instead of position *)

type counter = {
  tick : unit -> int;
  reset : unit -> unit;
}
  (* syntax for records uses {} and consists of declarations like " name : type " *)


let make_tick () : counter = 
  let count = ref 0 in {
    tick = (fun () -> let n = !count in count := !count + 1; n);
    reset = (fun () -> count := 0); 
  }
(* let { tick = t; reset = r} = make_tick() *)
(* infinite data structures -- lazy programming *)

type 'a susp = Susp of (unit -> 'a)

let force (s : 'a susp) : 'a = 
  let (Susp f) = s in 
  f()


let map (f : 'a -> 'b) (s : 'a susp) : 'b susp = 
  let (Susp g) = s in
  Susp(fun () -> f (g ()))

    (* OR *)

let map (f : 'a -> 'b) (s : 'a susp) : 'b susp = 
  Susp(fun () -> f (force s))


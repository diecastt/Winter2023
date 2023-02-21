let make_tick () =
  let count = ref 0 in
  ( (fun () -> count := !count + 1; !count),
    (fun () -> count := 0) )

let (tick, reset) = make_tick ()
    
let (tick', reset') = make_tick ()

    (* 1. syntax
   2. suspended computations
     3. streams *)

    (* record syntax *)

type counter = {
  tick : unit -> int;
  reset : unit -> unit;
}

    (* syntax for a record type uses {} and consists of
   declarations like `name : type` *)

let make_tick () : counter = (* make_tick is like an (OOP) constructor *)
  let count = ref 0 in {
    tick = (fun () -> let n = !count in count := !count + 1; n);
    reset = (fun () -> count := 0);
  } (* syntax for a record _value_ uses `=` but is otherwise the same. *)
  
    (* infinite data structures -- lazy programming: *)

(* A suspended computation that produces a value of type 'a. *)
type 'a susp = Susp of (unit -> 'a) 
  
   
   (* By the way, we can define a map-like function for any type that "holds"
   values. A function can be seen as "holding" a value in its output position.
*)
           
      (* omap : ('a -> 'b) -> 'a option -> 'b option *)
      (* nmap : ('a -> 'b) -> 'a nested -> 'b nested *)
                   
      (* A convenience function for running the suspended computation.
   We call it 'force' because the suspended computations are lazy! We have to
   "force" them to work. *)
let force (s : 'a susp) : 'a =
  let (Susp f) = s in
  f () 

    (* let x = map f (Susp (fun () -> longComputation ...)) *)
  
    (* streams
   
  infinite data structure
     defined by _observations_ rather than _constructors_ *)

type 'a str = {
  hd : 'a;
  tl : 'a str susp;
}

    (* We can observe the head of the stream, and get a value of type 'a.
   We can observe the tail of the stream, and get a suspended computation
   that gives us another stream. *)

    (* EXERCISE: define these functions *)
    (* nth : int -> 'a str -> 'a *)
    (*  ^ gets the nth element of the stream. *)
    (* skip : int -> 'a str -> 'a str *) 
    (* ^ skips the first n elements of the stream *)

    (* A map-like function for a suspended computation. *)
let susp_map (f : 'a -> 'b) (s : 'a susp) : 'b susp =
  Susp (fun () -> f (force s))
    (* Susp (fun () -> (fun x -> x + 2) (force (Susp (fun () -> uhoh 5)))) *)

    (* A map-like function for a stream. *)
let rec str_map (f : 'a -> 'b) (s : 'a str) : 'b str =
  { hd = f s.hd;
    tl = susp_map (str_map f) s.tl;
  } (* trace the call to susp_map here to see what the tail really is. *)

    (* Collects into a list the first n elements of the stream. *)
let rec take n (s : 'a str) : 'a list =
  if n = 0 then [] else
    s.hd :: take (n-1) (force s.tl)
      
      (* Solution to nth above. *)
let rec nth n s =
  if n = 0 then s.hd else
    nth (n-1) (force s.tl)
   
let rec repeat (x : 'a) : 'a str = {
  hd = x;
  tl = Susp (fun () -> repeat x) 
}    
let rec nats_from n = {
  hd = n;
  tl = Susp (fun () -> nats_from (n+1)) ;
} 
let rec iterate (f : 'a -> 'a) (x : 'a) : 'a str = {
  hd = x;
  tl = Susp (fun () -> iterate f (f x))
}

let nats_from' n = iterate (fun x -> x + 1) n
    (* CONVINCE YOURSELF that this definition does the same thing as our
direct implementation of `nats_from` above: trace one step of iterate *)
  
let fib = str_map fst (iterate (fun (a, b) -> (b, a+b)) (0, 1))
  

let rec add_streams (s1 : int str) (s2 : int str) : int str = {
  hd = s1.hd + s2.hd;
  tl = Susp (fun () -> add_streams (force s1.tl) (force s2.tl))
}
    (* CHALLENGE: implement add_streams using iterate and str_map *)
    
(*
0 1 1 2 3
+ + + + +
1 1 2 3 5
*)
let rec fib1 = {
  hd = 0;
  tl = Susp (fun () -> fib2);
}
and fib2 = {
  hd = 1;
  tl = Susp (fun () -> add_streams fib1 fib2);
}
    (* crazy lazy recursive definition of the fibonacci sequence *)

(* A fully lazy list.
Notice, if we have `l : 'a lazy_list`, then that's a suspended computation
    that when we run it, gives us an `'a lazy_list'` (notice the PRIME on the end
of that lazy_list there!).
  The 'a lazy_list' has one observation we can make unimaginatively called
  'uncons'. It asks "has the list ended yet?"
  If the list has ended, we get None.
                                  If the list hasn't ended, we get an 'a * 'a lazy_list, that is, a head element
and a tail. *)
type 'a lazy_list = 'a lazy_list' susp 
and 'a lazy_list' =
  { uncons : ('a * 'a lazy_list) option }
  
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree
  
(* Lazily flattening a tree. The suspended computation in each Cons cell of the
   lazy list stores the rest of the tree traversal that has yet to be performed.
   This combines ideas of lazy programming with ideas of CPS.
   FYI this is MUCH harder than the kinds of things you would be asked to do
   on a test. If you can understand this and come up with variants, then you're
   doing very well in understanding the concepts in the course. *)
let flatten (t : 'a tree) : 'a lazy_list =
  let rec go (t : 'a tree) (next : unit -> 'a lazy_list) : 'a lazy_list =
    match t with
    | Empty -> next ()
    | Node (l, x, r) -> 
        go l (fun () ->
            Susp (fun () -> { uncons = Some (x, go r next) }))
  in
  go t (fun () -> Susp (fun () -> { uncons = None } ))
                
    (* EXERCISE: implement take for lazy_list, and then make an example tree,
   flatten it, and take from that lazy list *)

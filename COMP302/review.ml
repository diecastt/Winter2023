(* PART 1: backtracking *)

(* Section 1.1: making change *)

exception Change

let canada_coins = [25;10;5]
let wacky_coins = [17;13;3]
let other_coins = [1;5;10]

let rec change (coins : int list) (amt : int) : int list =
  match coins with
  | _ when amt = 0 -> []
  | [] -> raise Change (* nonzero change to make but no more coins to try! *)
  | c :: cs when c > amt ->
     change cs amt
  | c :: cs ->
     try
       c :: change (c :: cs) (amt - c)
     with
     | Change -> change cs amt

       (* ORDER OF OPERATIONS:
           1. change (c :: cs) (amt - c)
           2. check whether that call succeeded/failed
                - if success:
                    tack c on the front, and return that successfully
                - if failed:
                    make a different recursive call *)

let rec change_opt (coins : int list) (amt : int) : int list option =
    match coins with
    | _ when amt = 0 -> Some []
    | [] -> None (* cause backtracking to the nearest match *)
    | c :: cs when c > amt -> change_opt cs amt
    | c :: cs ->
            (* set up a point to backtrack to *)
        match change_opt (c :: cs) (amt - c) with
        | Some soln -> Some (c :: soln)
        | None -> change_opt cs amt

let result = change_opt canada_coins 72

let rec change_cps (coins : int list) (amt : int)
    (return : int list -> 'r) (backtrack : unit -> 'r) : 'r =
    match coins with
    | _ when amt = 0-> return []
    | [] -> backtrack ()
    | c :: cs when c > amt -> change_cps cs amt return backtrack
    | c :: cs ->
        change_cps (c :: cs) (amt - c)
            (fun soln -> return (c :: soln))
            (fun () -> change_cps cs amt return backtrack)

let result = change_cps canada_coins 72 (fun soln -> Some soln) (fun () -> None)

(* Section 1.2: I can't get no satisfaction *)

type formula =
    | Conj of formula * formula
    | Disj of formula * formula
    | Neg of formula
    | Var of string

type truth_assignment = (string * bool) list

(* Evaluates a formula with a given assignment for the values of the variables. *)
let rec eval (env : truth_assignment) (phi : formula) : bool = failwith "as before"

(* All the variable names that appear in a formula. *)
let rec collect_variables (phi : formula) : string list = failwith "as before"

exception Unsatisfiable

let find_satisfying_assignment (phi : formula) : truth_assignment =
    let vars = collect_variables phi in
    let rec go (vars : string list) (ta : truth_assignment) : truth_assignment =
        match vars with
        | [] -> if eval ta phi then ta else raise Unsatisfiable
        | x :: xs ->
            try go xs ((x, true) :: ta) with
            | Unsatisfiable -> go xs ((x, false) :: ta)
            (* ORDER OF OPERATIONS:
                1. go xs ((x, true) :: ta)
                    - if succeeds: we succeed
                    - if fails: go xs ((x, false) :: ta) *)
    in
    go vars []

let find_satisfying_assignment_opt (phi : formula) : truth_assignment option =
    failwith "exercise"

let find_satisfying_assignment_cps (phi : formula)
    (return : truth_assignment -> 'r) (backtrack : unit -> 'r) : 'r =
    let vars = collect_variables phi in
    let rec go (vars : string list) (ta : truth_assignment)
        (backtrack : unit -> 'r) : 'r =
        match vars with
        | [] -> if eval ta phi then return ta else backtrack ()
        | x :: xs ->
            go xs ((x, true) :: ta)
                (fun () -> go xs ((x, false) :: ta) backtrack)
    in
    go vars [] backtrack

    (*
let result =
    find_satisfying_assignment_cps some_formula
        (fun soln -> Some soln)
        (fun () -> None)
        *)

(* PART 2: lazy programming with streams *)

type 'a susp = Susp of (unit -> 'a)
type 'a str = {
    hd : 'a;
    tl : 'a str susp;
}
let force (Susp f) = f ()

let rec take n s = if n = 0 then [] else s.hd :: take (n-1) (force s.tl)

let rec unfold (f : 'state -> 'elem * 'state) (s : 'state) : 'elem str =
    let (x, s') = f s in {
        hd = x;
        tl = Susp (fun () -> unfold f s')
    }

let rec map (f : 'a -> 'b) (s : 'a str) : 'b str = {
    hd = f s.hd;
    tl = Susp (fun () -> map f (force s.tl));
}

(* Section 2.1: Implement map for streams using `unfold`. *)
let rec map (f : 'a -> 'b) (s : 'a str) : 'b str =
    unfold (fun s -> (f s.hd, force s.tl)) s

let rec iterate (f : 'a -> 'a) (s : 'a) : 'a str = failwith "done before"

(* Section 2.2: Implement a specific stream. *)

(** The Wallis product, published in 1656, is an infinite product
    whose limit is pi/2. It is among the oldest known ways to
    calculate pi to arbitrary precision.

    It is defined as:
      (2/1 * 2/3) * (4/3 * 4/5) * (6/5 * 6/7) * (8/7 * 8/9) * ...

    Notice that the nth factor in the product is given by the formula:
      a_n = 2n/(2n-1) * 2n/(2n+1)
          = 4n^2/(4n^2 - 1)
    (Remark: this sequence begins at n=1 !)

    Denote by W_n the Wallis product truncated at factor n. So:
        W_1 = a_1 = 2/1 * 2/3
        W_2 = a_2 * W_1
        W_3 = a_3 * W_2 = a_3 * a_2 * a_1
    and so on.  *)

(** This previous method is quite inefficient, since it recalculates
    the same things over and over again, through the recursive function `f`.
    Recall:
      a_n = 4n^2/(4n^2 - 1)
    and
      W_(n+1) = a_n * W_n
    Using this, directly construct the stream
      wallis_2 : float str
    which contains all successive approximations of the Wallis product.
    Rank: **
   *)

let wallis_2 =
    let rec go n wn

(* PART 3: references and OOP *)

type state = Open | Closed
exception InvalidState

type door =
  { set : state -> unit
  ; pass : unit -> bool
  }

(* Write a function `make_door : unit -> door` that constructs an object of type `door`.

   The set method should adjust the state of the door.
   It should raise `InvalidState` if the door is already in the requested state.

   The function `pass` should return true if and only if one can pass
   through the door, i.e. if the door is open.

   The door should be initially closed. *)

let make_door () = failwith "exercise"

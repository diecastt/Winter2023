(* Hi everyone. All of these problems are generally "one-liners" and have slick solutions. They're quite cute to think
   about but are certainly confusing without the appropriate time and experience that you devote towards reasoning about
   this style. Good luck! :-)  *)

(* For example, if you wanted to use the encoding of five in your test cases, you could define: *)
let five : 'b church = fun s z -> s (s (s (s (s z))))
(* and use 'five' like a constant. You could also just use
 'fun z s -> s (s (s (s (s z))))' directly in the test cases too. *)

(* If you define a personal helper function like int_to_church, use it for your test cases, and see things break, you should
   suspect it and consider hard coding the input cases instead *)

(* Question 1a: Church numeral to integer *)
(* TODO: Test cases *)
let to_int_tests : (int church * int) list = [
]
;;

(* TODO: Implement
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems.
*)
let to_int (n : int church) : int = let inc n = n + 1 in n inc 0

(* Question 1b: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : ( ('b church * 'b church) * 'b church) list = [
]
;;

let add (n1 : 'b church) (n2 : 'b church) : 'b church =  n1 (fun f x -> f (n2 f x))

(* Question 1c: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : ( ('b church * 'b church) * 'b church) list = [
]
;;

let mult (n1 : 'b church) (n2 : 'b church) : 'b church =
  raise NotImplemented

(* Question 2a: Determine if a church numeral is even *)
(* TODO: Test cases *)
let is_even_tests : ('b church * bool) list = [
]
;;

let is_even (n : 'b church) : bool =
  raise NotImplemented

(* Question 2b: Generate a list whose length is given by a church numeral with one element over and over *)
(* TODO: Test cases;
 * You only need to test lists of int here. *)
let gen_list_tests : (('b church * int) * int list) list = [
]
;;

let gen_list (n : 'b church) (x : 'a) : 'a list =
  raise NotImplemented 
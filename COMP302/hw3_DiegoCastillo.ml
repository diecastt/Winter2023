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
  (five, 5);
  (zero, 0);
  (one, 1);
]
;;

(* TODO: Implement
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems.
*)
let to_int (n : int church) : int = n (fun x -> x + 1) 0


(* Question 1b: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : ( ('b church * 'b church) * 'b church) list = [
  ((zero, five), five);
  ((five, one), fun s z -> s (five s z));
  ((five, five), fun s z -> s (s (s (s (s (five s z))))) )
  
]
;;

let add (n1 : 'b church) (n2 : 'b church) : 'b church = fun s z -> n1 s (n2 s z)
    
(* Question 1c: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : ( ('b church * 'b church) * 'b church) list = [
  ((zero, five), zero);
  ((five, one), five);
  ((five, (fun s z -> s (one s z))), add five five)
  
]
;;

let mult (n1 : 'b church) (n2 : 'b church) : 'b church = fun s z -> n1 (n2 s) z
(* Question 2a: Determine if a church numeral is even *)
(* TODO: Test cases *)
let is_even_tests : ('b church * bool) list = [
  (zero, true);
  (one, false);
  ((fun s z -> s (one s z)), true);
  (five, false)
]
;;

let is_even (n : 'b church) : bool = 
  n not true

(* Question 2b: Generate a list whose length is given by a church numeral with one element over and over *)
(* TODO: Test cases;
 * You only need to test lists of int here. *)
let gen_list_tests : (('b church * int) * int list) list = [
  ((five, 2), [2;2;2;2;2]);
  ((zero, 4), []);
  ((one, 39), [39]);
  (((add five five), 3), [3;3;3;3;3;3;3;3;3;3])
]
;;

let gen_list (n : 'b church) (x : 'a) : 'a list =
  n (fun z -> z @ [x]) []

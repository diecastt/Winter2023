(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for tree depth. *)
let tree_depth_cps_tests : (int tree * int) list =
  [
    (Empty, 0);
    ((Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty))), 2);
    ((Tree (Empty, 1, Tree (Empty, 3, Empty))), 2);
    ((Tree (Empty, 2, Empty)), 1);
    ((Tree (Tree (Tree (Empty, 3, Empty), 2, Empty), 1, Empty)), 3);
    ((Tree (Tree (Empty, 2, Empty), 1, Empty)), 2);
    ((Tree (Tree (Tree (Empty, 3, Empty), 2, Empty), 1, Tree (Tree (Empty, 3, Empty), 2, Empty))), 3);
    
  ]

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth t =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* TODO: Implement a CPS style tree_depth_cps function.*)
let tree_depth_cps (t: 'a tree) = 
  let rec helper (t: 'a tree) (sc: (int -> int)) =
    match t with
    | Empty -> sc 0
    | Tree (l, _, r) -> helper l (fun heightl -> 
        helper r ( fun heightr -> 
            sc (1 + max heightl heightr))) 
  in
  helper t (fun x -> x)

(* Question 2(a): Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *)
let traverse_tests : (int tree * int list) list = [
  ((Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty))), [2;3;1]);
  ((Tree (Tree (Tree (Empty, 4, Empty), 2, Tree (Empty, 5, Empty)), 1, Tree (Empty, 3, Empty))), [4; 5; 2; 3; 1])

]

(* TODO: Implement a CPS style postorder traversal function. *)
let traverse (tree : 'a tree) : 'a list =
  let rec helper (tree : 'a tree) (sc : 'a list -> 'r) =
    match tree with
    | Empty -> sc []
    | Tree (l, x, r) -> helper l (fun left -> 
        helper r (fun right -> 
            sc ((left @ right) @ [x])))
  in
  helper tree (fun x -> x)
  
   

(* Question 2(b): Distances from the Root *)
(* TODO: Write a good set of tests for testing the get_distances function. *)
let get_distances_tests : (int tree * int list) list = [
  ( Tree (Tree (Empty, 3, Empty), 5, Tree (Empty, 6, Empty)), [8; 11; 5]);
  ( (Tree (Tree (Empty, 2, Tree (Empty, 6, Empty)), 3, Tree (Empty, 4, Empty))), [11; 5; 7; 3] )


]

(* TODO: Implement a CPS style get_distances function. *)
let get_distances (tree : int tree) : int list = 
  let rec helper tree sum sc =
    match tree with
    | Empty -> sc []
    | Tree (l, x, r) -> helper l (sum + x) (fun left -> 
        helper r (sum + x) (fun right -> 
            sc ((left @ right) @ [sum + x])))
  in
  helper tree 0 (fun x -> x)
  

(* Question 3: Finding Subtrees *)
(* TODO: Write a good set of tests for finding subtrees. *)

(* Defining tree for Q3 test cases *)
let q3_tree = Tree (
    Tree (Tree(Empty, 4, Empty), 6, Tree(Empty, 8, Empty)), 1, 
    Tree(Empty, 7, Empty))
    
let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [
    (([1], q3_tree),  Some (Tree (Tree (Empty, 4, Empty), 6, Tree(Empty, 8, Empty))));
    (([1;6], q3_tree), Some (Tree(Empty, 4, Empty)));
    (([1;7], q3_tree), Some Empty);
    ( ([23; 55], q3_tree), None);
  ]

(* TODO: Implement a CPS style find_subtree_cont function.*)
let find_subtree_cps ls tree =
  let rec helper ls tree sc fc = 
    match (ls, tree) with
    | [], x -> sc  (Some x)
    | ((x :: xs), Tree(l, v, r)) when x = v -> helper xs l sc (fun () -> helper xs r sc fc)
    | ((_ :: _), _) -> fc ()
  in
  helper ls tree (fun x -> x) (fun () -> None) 
  
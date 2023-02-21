(*--------------------------------------------------------------*)
(* Q1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s)

(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  let string_list = List.map (Char.escaped) l in
  List.fold_left (^) "" string_list

(* ------------------------------------------------------------------------*)
(* Q2 : Bank Account *)
(* ------------------------------------------------------------------------*)

let open_account (pass: password) : bank_account = 
  let pswd = ref pass in
  let balance = ref 0 in
  let wrong_count = ref 0 in
  let locked = ref false in
  {
    update_pass = (fun oldpass newpass -> 
        if !locked then raise account_locked;
        if not (oldpass = !pswd) && (!wrong_count + 1 = 3) then (locked := true; raise wrong_pass)
        else if not (oldpass = !pswd) then (wrong_count := !wrong_count + 1; raise wrong_pass)
        else wrong_count := 0; pswd := newpass);
    
    retrieve = (fun pass amount -> 
        if !locked then raise account_locked;
        if not (pass = !pswd) && (!wrong_count + 1 = 3) then (locked := true; raise wrong_pass);
        if not (pass = !pswd) then (wrong_count := !wrong_count + 1; raise wrong_pass)
        else if amount < 0 then (wrong_count := 0; raise negative_amount)
        else if amount > !balance then (wrong_count := 0; raise not_enough_balance)
        else wrong_count := 0; balance := !balance - amount);
            
    
    deposit = (fun pass amount -> 
        if !locked then raise account_locked;
        if not (pass = !pswd) && (!wrong_count + 1 = 3) then (locked := true; raise wrong_pass);
        if not (pass = !pswd) then (wrong_count := !wrong_count + 1; raise wrong_pass)
        else if amount < 0 then (wrong_count := 0; raise negative_amount)
        else wrong_count := 0; balance := !balance + amount);
    
    show_balance = (fun pass -> 
        if !locked then raise account_locked;
        if not (pass = !pswd) && (!wrong_count + 1 = 3) then (locked := true; raise wrong_pass);
        if not (pass = !pswd) then (wrong_count := !wrong_count + 1; raise wrong_pass)
        else wrong_count := 0; !balance);
  } 
;;

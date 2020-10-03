(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)
(*works*)
let rev_tup tup = 
  match tup with 
  (a,b,c) -> (c,b,a)
;;
(*works*)
let abs x =
  let neg_to_abs x = -1 * x in 
  let pos_to_abs x = x in 
  if x < 0 then neg_to_abs x 
  else          pos_to_abs x 
;;

(*works*)
let area x y = 
  match x with 
  (a,b) -> (match y with 
          (d,e) -> abs (d-a) * abs (e-b) )
;;

(*works*)
let volume x y = 
  match x with 
  (a,b,c) -> match y with 
  (d,e,f) -> abs (d-a) * abs (e-b) * abs (f-c)
;;

(*works*)
let equiv_frac (a, b) (x, y) = 
  if b = 0 || y = 0 then 
    false
  else 
    let frac1 = float (a) /. float (b) in 
    let frac2 = float (x) /. float(y) in 
    if frac1 = frac2 then true else false 
    ;;
;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

(*WORK BUT: Stack Overflow->learn why*)
let rec factorial x = 
  match x with 
  |0 -> 1             (*Why should this be with base case 1 and not 0???*)
  |_ -> x * factorial (x-1)
;;

(*Learn why this work but still stack-overflow*)
let rec pow x y = 
  match y with 
  |0 -> 1
  |_ -> x * pow x (y-1)   (*Why put brackets around parameters tho?? Why this not cause stack overflow???*)
  (*don't put comma and brackets it will make a tuple*)
;;

(*helper method for tail*)
let rec tail_helper x num str_holder = 
  
  if x = 0 then 
    str_holder
  else
    if num = 0 then 
      str_holder
    else
      let rem = x mod 10 in 
      let rem_str = string_of_int rem in 
      tail_helper (x/10) (num-1) rem_str^str_holder   
      (*woaw just putting all your params into individual groupings worsk*)
;;

(*unbound_type err = haven't declared f() yet*)
(*WORKS*)
let rec tail x num = 
  let tail_nums_str = tail_helper x num "" in 
    if tail_nums_str = "" then 
      0
    else
      int_of_string tail_nums_str
  ;;
;;

(*WORKS*)
let rec len x = 

  if x/10 = 0 then 1
  else
    1 + len (x/10)
;;

(*fucking done baby*)
let rec contains_helper sub pattern_present curr_sub curr_val = 
  
  if curr_sub = 0 && pattern_present then 
    true
  else 
    if curr_val = 0 && pattern_present = false then 
      false
    else 
      if pattern_present then 
        if curr_sub mod 10 = curr_val mod 10 then 
          contains_helper (sub) (true) (curr_sub/10) (curr_val/10)
        else 
          contains_helper (sub) (false) (sub) (curr_val/10)
      else
        if curr_sub mod 10 = curr_val mod 10 then 
          contains_helper (sub) (true) (sub/10) (curr_val/10)
        else
          contains_helper (sub) (false) (sub) (curr_val/10)
;;

(*Seems like it works*)
let rec contains sub x = 

  contains_helper sub false sub x
;;


(*****************)
(* Part 3: Lists *)
(*****************)

(*DON'T WORK IDK WHY*)
(* let rec get idx lst = 
  match lst with 
  |[] -> if idx > 0 then failwith "Out of Bounds"
          else []
  |(h::t) -> if idx > 0 then get (idx-1) t 
              else h
;; *)

(*WORKS*)
let rec get idx lst = 
  match (idx, lst) with 
  | (_, []) -> failwith "Out of bounds"
  | (0,(h::_)) -> h
  | (_, (_::t)) -> get (idx-1) t
;;
let add head total_list = 
  head::total_list
;;
let r_add total_list head = 
  head::total_list
;;
let rec foldl func acc lst = 
  match lst with 
  | [] -> acc
  | h::t -> foldl func (func acc h) t
;;

let rec foldr func lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> func h (foldr add t acc)
;;

let rec combine lst1 lst2 = 
  let list1 = [] in 
  let combo = foldr add list1 lst2 in 
  foldr add lst1 combo
  ;;
;;

let rec reverse lst = 
  let lister = [] in 
  let func = r_add in 
  foldl func lister lst
  ;;
;;
let rec length_list lst = 
  match lst with 
  | [] -> 0
  | h::t -> 1 + length_list t
;;
let rec front_taker count lst acc =
  match count with 
  | 0 -> acc
  | _ -> match lst with 
        | [] -> acc
        | h::t -> front_taker (count - 1) (t) (h::acc)
;;
let rec back_taker_helper lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> back_taker_helper t (h::acc)
;;
let rec back_taker count lst acc = 
  
  if count > 0 then 
    match lst with 
    | [] -> acc
    | (h::t) -> back_taker (count - 1) t acc
  else 
    back_taker_helper lst []
;;

let rec rotate shift lst = 
  let lst_len = length_list lst in 
  if lst_len = 0 then [] 
  else
    let front_end_rev = front_taker (shift mod lst_len) lst [] in  
    let back_end_rev = back_taker (shift mod lst_len) lst [] in 
    let front_end = reverse front_end_rev in 
    let back_end = reverse back_end_rev in 
    combine back_end front_end
;;
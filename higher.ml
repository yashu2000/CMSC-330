open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = 
  match lst with 
  | [] -> 0
  | _ -> fold (fun acc h -> if h = target then acc + 1 else acc) 0 lst
;;

let uniq lst = fold (fun acc h -> if count_occ (acc) (h) = 0 then (h::acc) else acc) [] lst;;

let assoc_list lst = 
  fold (fun acc h -> let adder = (h, count_occ lst h) in 
                     if count_occ acc adder > 0 then acc else adder::acc 
  ) [] lst
;;

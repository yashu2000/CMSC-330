open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf                               (*effectively "null" -> what a leaf points to*)
  | IntNode of int * int_tree * int_tree  (*When the node is a root to a subtree*)

let empty_int_tree = IntLeaf

(*Inserts a node into BST*)
let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)
;;
let rec int_insert_all lst tree = 
  match lst with 
  | (h::t) -> int_insert_all t (int_insert h tree)
  | _ -> tree
;;
(*Finds target value in tree*)
let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l
;;
(* Implement the functions below. *)

let rec int_size t = 
  match t with 
  | IntLeaf -> 0
  | IntNode (root, left, right) -> 1 + int_size left + int_size right
;;

(*right like this-> efficiency problems??*)
(*how test??*)
(*gives invalid arg err everytime when we try to nest additions*)
let rec int_max t = 
  match t with 
  | IntLeaf -> invalid_arg "int_max"
  | IntNode (leafer, _, IntLeaf) -> leafer
  | IntNode (root, _, right) -> int_max right
;;

let rec int_com_helper t x y = 

  match t with 
    | IntLeaf -> invalid_arg "int_common"
    | IntNode (root, left, right) when (x < root && y > root || x > root && y < root) -> root
    | IntNode (root, left, right) when x > root && y > root -> int_com_helper right x y
    | IntNode (root, left, right) when x < root && y < root -> int_com_helper left x y
    | IntNode (root, left, right) when x = root || y = root -> root
    | _ -> x (*can be case where x = y or*)
;;

let rec int_common t x y = 
  let x_present = int_mem x t in 
  let y_present = int_mem y t in 

  if x_present && y_present then 
    int_com_helper t x y
  else
    invalid_arg "int_common"
;;
(*End of Part 2*)
(*-----------------------------------------------------------------------------------*)
(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert x t = 
  let (comp, tree) = t in 
  match tree with 
  | Leaf -> (comp, Node (x, Leaf, Leaf))
  (*Need to make new tuple again because we don't want to return "the inserted element"*)
  (*we want to return the whole subtree -> so deconstruct and reconstruct*)
  | Node (root, left, right) when comp x root > 0 -> let (_, right) = pinsert x (comp, right) in
  (comp, Node (root, left, right))

  (*dont just compare to any tuple -> compare to a 'a atree Node tuple*)
  | Node (root, left, right) when comp x root < 0 -> let (_, left) = pinsert x (comp, left) in 
  (comp, Node (root, left, right))

  | Node (root, left, right) -> let (_, right) = pinsert x (comp, right) in (*therefore its equal to 0*)
  (comp, Node (root, left, right))
;;

let rec pmem x t =
  let (comp, tree) = t in 
  match tree with 
  | Leaf -> false
  | Node (root, left, right) when comp x root > 0 -> pmem x (comp, right)
  | Node (root, left, right) when comp x root < 0 -> pmem x (comp, left)
  | Node (root, left, right) when comp x root = 0 -> true
  | _ -> false
;;

(*problem with pinsert??*)
let rec pinsert_all lst t = 
  match lst with 
  | [] -> t
  | (head::tail) -> (pinsert_all tail (pinsert head t))
;;

(*@ -> adds two lists together*)
let rec p_as_list t = 
  let (comp, tree) = t in
  match tree with 
  | Node (root, left, right) -> (p_as_list (comp,left) @ ([root] @ (p_as_list (comp,right) @ [])))
  | _ -> []
;;

(*doesnt work in second input*)
let rec pmap f t =
  let (comp, tree) = t in 
  let lst_of_tree = map f (p_as_list t) in 
  pinsert_all lst_of_tree (empty_ptree comp)
;;
(*-----------------------------------------------------------------------------------*)
(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(*variant = the number of different types of values the user defined type can become*)
(**)
(*dont need to define 2 variants*)
type lookup_table = (string * int) list list


let empty_table () : lookup_table = 
  []  (*Can infer that within this empty list we are going to put in (string*int) list s*)
;;
let push_scope (table: lookup_table) : lookup_table = 
  let new_scope = [] in 
  new_scope::table
;;
let pop_scope (table: lookup_table) : lookup_table = 
  match table with
  | (h::t) -> t
  | _ -> failwith "No scopes remain!"
;;
let add_var name value (table: lookup_table) : lookup_table = 
  match table with 
  | (h::t) -> ((name,value)::h)::t
  | _ -> failwith "There are no scopes to add a variable to!"
;;

let rec lookup_in_scope_block scope var_name =
  match scope with
  | (h::t) -> let (name, value) = h in 
              if name = var_name then (true , value)
              else lookup_in_scope_block t var_name
  | [] -> (false, 0)
;;
let rec lookup name (table: lookup_table) = 
  match table with 
  | (scope_head::rest_of_scopes) -> let (found, var) = lookup_in_scope_block scope_head name in 
                                    if found = true then var else lookup name rest_of_scopes
  | _ -> failwith "Variable not found!"

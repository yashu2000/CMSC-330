open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec lookup env var = 
  match env with 
  | [] -> raise (DeclareError "no variable binding")
  | (name, value)::env' -> if name = var then value else lookup env' var
;;

let rec eval_expr env expr =
  match expr with 
  | ID var1 -> lookup env var1 
  | Int v -> (Int_Val v)
  | Bool v -> (Bool_Val v)
  | Add (e1, e2) -> let v1 = eval_expr env e1 in  (*run compiler typeerror might not work for tests have to see*)
                    let v2 = eval_expr env e2 in 
                    (match v1, v2 with 
                    | Int_Val n1, Int_Val n2 -> let n3 = n1 + n2 in (Int_Val n3)
                    | _-> raise (TypeError "type mismatch"))

  | Sub (e1, e2) -> let v1 = eval_expr env e1 in 
                    let v2 = eval_expr env e2 in 
                    (
                    match v1, v2 with 
                    | Int_Val n1, Int_Val n2 -> let n3 = n1 - n2 in (Int_Val n3)
                    | _ -> raise (TypeError "type mismatch")
                    )

  | Mult (e1, e2) -> let v1 = eval_expr env e1 in 
                     let v2 = eval_expr env e2 in 
                     (
                     match v1, v2 with 
                     | Int_Val n1, Int_Val n2 -> let n3 = n1 * n2 in (Int_Val n3)
                     | _ -> raise (TypeError "type mismatch")
                     )

  | Div (e1, e2) -> let v1 = eval_expr env e1 in 
                    let v2 = eval_expr env e2 in 
                    (
                    match v1, v2 with 
                    | Int_Val n1, Int_Val n2 -> if n2 = 0 then raise (DivByZeroError) else (let n3 = floor ((float_of_int n1) /. (float_of_int n2)) in 
                                                                                           (Int_Val (int_of_float n3)))
                    | _ -> raise (TypeError "type mismatch")
                    )
  
  | Pow (e1, e2) -> let v1 = eval_expr env e1 in 
                    let v2 = eval_expr env e2 in 
                    (
                    match v1, v2 with 
                    | Int_Val n1, Int_Val n2 -> let n3 = floor ((float_of_int n1) ** (float_of_int n2)) in (Int_Val (int_of_float n3))
                    | _ -> raise (TypeError "type mismatch")
                    )
    
  | Not e1 -> let v1 = eval_expr env e1 in (match v1 with 
                                           | Bool_Val b1 -> Bool_Val (not(b1))
                                           | _ -> raise (TypeError "type mismatch"))

  | Equal (e1, e2) -> let val1 = eval_expr env e1 in
                      let val2 = eval_expr env e2 in
                      (match val1, val2 with 
                      | Int_Val v1, Int_Val v2 -> if v1 = v2 then Bool_Val true else Bool_Val false
                      | Bool_Val b1, Bool_Val b2 -> if b1 = b2 then Bool_Val true else Bool_Val false
                      | _ -> raise (TypeError "type mismatch")    
                      )
  | NotEqual (e1, e2) -> let val1 = eval_expr env e1 in
                         let val2 = eval_expr env e2 in
                         ( match val1, val2 with 
                           | Int_Val v1, Int_Val v2 -> if v1 != v2 then Bool_Val true else Bool_Val false
                           | Bool_Val b1, Bool_Val b2 -> if b1 != b2 then Bool_Val true else Bool_Val false
                           | _ -> raise (TypeError "type mismatch")
                         )

  | Greater (e1, e2) -> let v1 = eval_expr env e1 in 
                        let v2 = eval_expr env e2 in 
                        (
                        match v1, v2 with 
                        | Int_Val n1, Int_Val n2 -> if n1 > n2 then (Bool_Val true) else (Bool_Val false)
                        | _ -> raise (TypeError "type mismatch")
                        )
                        

  | Less (e1, e2) -> let v1 = eval_expr env e1 in 
                     let v2 = eval_expr env e2 in 
                     (
                     match v1, v2 with 
                     | Int_Val n1, Int_Val n2 -> if n1 < n2 then (Bool_Val true) else (Bool_Val false) 
                     | _ -> raise (TypeError "type mismatch")
                     )
                     
  | GreaterEqual (e1, e2) -> let v1 = eval_expr env e1 in 
                             let v2 = eval_expr env e2 in 
                             (
                             match v1, v2 with 
                             | Int_Val n1, Int_Val n2 -> if n1 >= n2 then (Bool_Val true) else (Bool_Val false)
                             | _ -> raise (TypeError "type mismatch")
                             )
                              

  | LessEqual (e1, e2) -> let v1 = eval_expr env e1 in 
                          let v2 = eval_expr env e2 in 
                          (
                          match v1, v2 with 
                          | Int_Val n1, Int_Val n2 -> if n1 <= n2 then Bool_Val true else Bool_Val false
                          | _ -> raise (TypeError "type mismatch")
                          )
                          

  | Or (e1, e2) -> let v1 = eval_expr env e1 in 
                   let v2 = eval_expr env e2 in 
                   (
                   match v1, v2 with 
                   | Bool_Val b1, Bool_Val b2 -> if b1 || b2 then Bool_Val true else Bool_Val false
                   | _ -> raise (TypeError "type mismatch")
                   )
                    

  | And (e1, e2) -> let v1 = eval_expr env e1 in 
                    let v2 = eval_expr env e2 in 
                    (
                    match v1, v2 with 
                    | Bool_Val b1, Bool_Val b2 -> if b1 && b2 then Bool_Val true else Bool_Val false
                    | _ -> raise (TypeError "type mismatch")
                    )
                    
  | _ -> raise (TypeError "type mismatch")

;;


let rec eval_stmt env stmt = 
  let rec assign_helper new_v old_v = 
    match (new_v, old_v) with 
    | Int_Val _, Int_Val _ -> true
    | Bool_Val _, Bool_Val _ -> true
    | _ -> false
  in
  match stmt with 
  | NoOp -> env
  | Seq (stmt1, stmt2) -> let env' = eval_stmt env stmt1 in eval_stmt env' stmt2
  | Declare (typer, id) -> if List.mem_assoc id env then raise (DeclareError "pre-declared id") else 
                           (match typer with 
                           | Int_Type -> (id, (Int_Val 0))::env
                           | Bool_Type -> (id, (Bool_Val false))::env )
                  
  | Assign (id, e1) -> let v = eval_expr env e1 in 
                       if List.mem_assoc id env then 
                          if assign_helper v (List.assoc id env) then 
                            let env' = List.remove_assoc id env in 
                            (id, v)::env'
                          else raise (TypeError "type mismatch")
                       else raise (DeclareError "undeclared id")

  | If (e1, stmt1, stmt2) -> let v = eval_expr env e1 in 
                             (match v with 
                             | Bool_Val truer -> if truer then eval_stmt env stmt1 else eval_stmt env stmt2
                             | Int_Val _ -> raise (TypeError "type mismatch") )
          
  | While (e1, stmt1) -> let v = eval_expr env e1 in 
                         (match v with 
                         | Bool_Val truer -> if truer then eval_stmt (eval_stmt env stmt1) stmt else env
                         | Int_Val _ -> raise (TypeError "type mismatch") )

  | For (id, b_expr, e_expr, stmt1) -> let e_val = eval_expr env e_expr in 
                                       let b_val = eval_expr env b_expr in 
                                       (match (e_val, b_val) with 
                                        | Int_Val e_num, Int_Val b_num ->
                                          let env' = eval_stmt env (Assign (id, (Int b_num))) in
                                          if b_num <= e_num then 
                                            let env'' = eval_stmt env' stmt1 in 
                                            let (Int_Val id_num) = List.assoc id env'' in
                                            eval_stmt env'' (For (id, Int (id_num+1), e_expr, stmt1))
                                          else env'
                                        | _ -> raise (TypeError "type mismatch") 
                                       )

  | Print p1 -> let vp1 = eval_expr env p1 in 
                (match vp1 with 
                | Int_Val v -> (print_output_int v; print_output_newline (); env)
                | Bool_Val b -> (print_output_bool b; print_output_newline (); env))

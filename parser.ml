open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse1 toks
  and parse1 tokens =
    let tok, and1 = parse2 tokens in
    match (lookahead tok) with
    | Tok_Or -> let tok1 = match_token tok Tok_Or in
                  let tok2, or1 = parse1 tok1
                  in (tok2, Or (and1, or1))
    | _ -> tok, and1

  and parse2 tokens =
    let tok, eq1 = parse3 tokens in
    match (lookahead tok) with
    | Tok_And ->let tok1 = match_token tok Tok_And in
                  let tok2, and1 = parse2 tok1
                  in (tok2, And (eq1, and1))
    | _ -> tok, eq1

  and parse3 tokens =
    let tok, rl1 = parse4 tokens in
    match (lookahead tok) with
    | Tok_NotEqual -> let tok1 = match_token tok Tok_NotEqual in
                      let tok2, eq1 = parse3 tok1
                      in (tok2, NotEqual (rl1, eq1))
    | Tok_Equal -> let tok1 = match_token tok Tok_Equal in 
                   let tok2, eq1 = parse3 tok1 in 
                   (tok2, Equal (rl1, eq1))
    | _ -> tok, rl1

  and parse4 tokens =
    let tok, add1 = parse5 tokens in
    match (lookahead tok) with
    | Tok_Greater -> let tok1 = match_token tok Tok_Greater in
                     let tok2, rl1 = parse4 tok1 in 
                     (tok2, Greater (add1, rl1))

    | Tok_GreaterEqual -> let tok1 = match_token tok Tok_GreaterEqual in 
                          let tok2, rl1 = parse4 tok1 in 
                          (tok2, GreaterEqual (add1, rl1))

    | Tok_Less -> let tok1 = match_token tok Tok_Less in 
                  let tok2, rl1 = parse4 tok1 
                  in (tok2, Less (add1, rl1))

    | Tok_LessEqual -> let tok1 = match_token tok Tok_LessEqual in 
                       let tok2, rl1 = parse4 tok1 in 
                       (tok2, LessEqual (add1, rl1))

    | _ -> tok, add1

  and parse5 tokens =
    let tok, multi1 = parse6 tokens in
    match (lookahead tok) with
    | Tok_Sub -> let tok1 = match_token tok Tok_Sub in 
                 let tok2, add1 = parse5 tok1 in 
                 (tok2, Sub (multi1, add1))
    | Tok_Add -> let tok1 = match_token tok Tok_Add in 
                 let tok2, add1 = parse5 tok1 in 
                 (tok2, Add (multi1, add1))
    | _ -> tok, multi1

  and parse6 tokens =
    let tok, pwr1 = parse7 tokens in
    match (lookahead tok) with
    | Tok_Div -> let tok1 = match_token tok Tok_Div in 
                 let tok2, multi1 = parse6 tok1 in 
                 (tok2, Div (pwr1, multi1))
    | Tok_Mult -> let tok1 = match_token tok Tok_Mult in 
                  let tok2, multi1 = parse6 tok1 in 
                  (tok2, Mult (pwr1, multi1))
    | _ -> tok, pwr1

  and parse7 tokens =
    let tok, una1 = parse8 tokens in
    match (lookahead tok) with
    | Tok_Pow -> let tok1 = match_token tok Tok_Pow in 
                 let tok2, pwr1 = parse7 tok1 in 
                 (tok2, Pow (una1, pwr1))
    | _ -> tok, una1

  and parse8 tokens =
    match (lookahead tokens) with
    | Tok_Not -> let tok = match_token tokens Tok_Not in 
                 let tok1, una1 = parse8 tok in 
                 (tok1, Not una1)
    | _ -> parse9 tokens

  and parse9 tokens =
    match (lookahead tokens) with
    | Tok_Bool temp -> let tok = match_token tokens (Tok_Bool temp) in 
                       (tok, Bool temp)
    | Tok_Int temp -> let tok = match_token tokens (Tok_Int temp) in 
                      (tok, Int temp)
    | Tok_ID temp -> let tok = match_token tokens (Tok_ID temp) in 
                     (tok, ID temp)
    | Tok_LParen -> let tok = match_token tokens Tok_LParen in 
                    let tok1, eq1 = parse_expr tok in
                    let tok2   = match_token tok1 Tok_RParen in 
                    (tok2, eq1)
    | _ -> raise (InvalidInputException "Incorrect Expression")


let rec parse_stmt toks : stmt_result =
  let match_id tokie = match (lookahead tokie) with
                        | Tok_ID id -> (match_token tokie (Tok_ID id), id)
                        | _ -> raise (InvalidInputException "Incorrect ID")
  in

  let parse_dec tokie alt_tok alt_tok_o = let tok = match_token tokie alt_tok in 
                                          let tok1, i = match_id tok in 
                                          let tok2 = match_token tok1 Tok_Semi in 
                                          let r1, r2 = parse_stmt tok2 in 
                                          (r1, Seq (Declare (alt_tok_o, i), r2)) 
  in

  let parse_ass tokie = let tok, i  = match_id tokie in 
                           let tok1 = match_token tok Tok_Assign in
                           let tok2, ass1 = parse_expr tok1 in
                           let tok3 = match_token tok2 Tok_Semi in
                           let r1, r2 = parse_stmt tok3 in 
                           (r1, Seq (Assign (i, ass1), r2))
  in
  let parse_print tokie = let tok = match_token tokie Tok_Print in 
                          let tok1 = match_token tok Tok_LParen in 
                          let tok2, pr1 = parse_expr tok1 in 
                          let tok3 = match_token tok2 Tok_RParen in 
                          let tok4 = match_token tok3 Tok_Semi in 
                          let r1, r2 = parse_stmt tok4 in 
                          (r1, Seq (Print pr1, r2))
  in
  let parse_if tokie = let tok = match_token tokie Tok_If in 
                       let tok1 = match_token tok Tok_LParen in
                       let tok2, if1 = parse_expr tok1 in
                       let tok3 = match_token tok2 Tok_RParen in
                       let tok4 = match_token tok3 Tok_LBrace in
                       let alt_tok, x = parse_stmt tok4 in
                       let alt_tok1 = match_token alt_tok Tok_RBrace in
                       match (lookahead alt_tok1) with
                       | Tok_Else -> let alt_tok2 = match_token alt_tok1 Tok_Else in
                                     let alt_tok3 = match_token alt_tok2 Tok_LBrace in
                                     let alt_tok4, x1 = parse_stmt alt_tok3 in 
                                     let alt_tok5 = match_token alt_tok4 Tok_RBrace in 
                                     let r1, r2 = parse_stmt alt_tok5 in 
                                     (r1, Seq (If (if1, x, x1), r2))
                       | _ -> let r1, r2 = parse_stmt alt_tok1 in 
                              (r1, Seq (If (if1, x, NoOp), r2))
  in
  let parse_for tokie = let tok = match_token tokie Tok_For in
                        let tok1 = match_token tok Tok_LParen in
                        let tok2, i = match_id tok1 in
                        let tok3 = match_token tok2 Tok_From in
                        let tok4, for1  = parse_expr tok3 in
                        let alt_tok = match_token tok4 Tok_To in
                        let alt_tok1, for2 = parse_expr alt_tok in
                        let alt_tok2 = match_token alt_tok1 Tok_RParen in
                        let alt_tok3 = match_token alt_tok2 Tok_LBrace in
                        let alt_tok4, x = parse_stmt alt_tok3 in 
                        let alt_tok5 = match_token alt_tok4 Tok_RBrace in 
                        let r1, r2 = parse_stmt alt_tok5 in 
                        (r1, Seq (For (i, for1, for2, x), r2))
  in
  let parse_while tokie = let tok = match_token tokie Tok_While in 
                          let tok1 = match_token tok Tok_LParen in 
                          let tok2, wh1 = parse_expr tok1 in 
                          let tok3 = match_token tok2 Tok_RParen in 
                          let tok4 = match_token tok3 Tok_LBrace in 
                          let alt_tok, x = parse_stmt tok4 in 
                          let alt_tok1 = match_token alt_tok Tok_RBrace in 
                          let r1, r2 = parse_stmt alt_tok1 in 
                          (r1, Seq (While (wh1, x), r2))
  in
  match lookahead toks with
  | EOF 
  | Tok_Bool_Type    -> parse_dec toks Tok_Bool_Type Bool_Type
  | Tok_If           -> parse_if toks
  | Tok_RBrace -> (toks, NoOp)
  | Tok_Int_Type     -> parse_dec toks Tok_Int_Type Int_Type
  | Tok_For          -> parse_for toks
  | Tok_While        -> parse_while toks
  | Tok_ID _         -> parse_ass toks
  | Tok_Print        -> parse_print toks
  | _ -> raise (InvalidInputException "Incorrect Statement")


let parse_main toks : stmt =
  let tok = match_token toks Tok_Int_Type in
  let tok1 = match_token tok Tok_Main in
  let tok2 = match_token tok1 Tok_LParen in
  let tok3 = match_token tok2 Tok_RParen in
  let tok4 = match_token tok3 Tok_LBrace in
  let (tok_alt, x) = parse_stmt tok4 in 
  let tok_alt1 = match_token tok_alt Tok_RBrace in
  if tok_alt1 = [EOF] then x 
  else raise (InvalidInputException "End Of File Has Not Been Reached")

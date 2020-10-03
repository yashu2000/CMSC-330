open TokenTypes
open String
open Str

let re_space = Str.regexp "[ \n\r\t]+"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_equal = Str.regexp "=="
let re_notequal = Str.regexp "!="
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "!"
let re_semi = Str.regexp ";"
let re_int = Str.regexp "-?[0-9]+"
let re_int_type = Str.regexp "int"
let re_bool = Str.regexp "true\\|false" 
let re_bool_type = Str.regexp "bool"
let re_print = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_for = Str.regexp "for"
let re_from = Str.regexp "from"
let re_to = Str.regexp "to"
let re_while = Str.regexp "while"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_pow = Str.regexp "\\^"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"

let tokenize (input : string) : token list =

  let rec tok pos str = 
    
    let tok_helper key_word token pos str = 

      if (Str.string_match re_id str pos) then 
        let matched = Str.matched_string str in 
        
        if (String.length key_word) < (String.length matched) then 
          (Tok_ID matched)::(tok (pos + (String.length matched)) str)
        
        else
          token::(tok (pos + (String.length key_word)) str)
      else 
        token::(tok (pos + (String.length key_word)) str)
    in

    if pos >= (String.length str) then [EOF]
    
    (*Skip whitespace*)
    else if (Str.string_match re_space str pos) then 
      (tok (pos + (String.length (Str.matched_string str))) str)
    
    (*types*)
    else if (Str.string_match re_bool str pos) then 
      let matched = Str.matched_string str in
      tok_helper matched (Tok_Bool (bool_of_string (matched))) pos str 
    
    else if (Str.string_match re_int str pos) then 
      let matched = Str.matched_string str in 
      (Tok_Int (int_of_string matched))::(tok (pos + (String.length matched)) str)
    
    (*symbols*)
    else if (Str.string_match re_semi str pos) then 
      Tok_Semi::(tok (pos + 1) str)

    else if (Str.string_match re_rbrace str pos) then 
      Tok_RBrace::(tok (pos+1) str)
    
    else if (Str.string_match re_lbrace str pos) then 
      Tok_LBrace::(tok (pos+1) str)

    else if (Str.string_match re_rparen str pos) then 
      Tok_RParen::(tok (pos+1) str)

    else if (Str.string_match re_lparen str pos) then 
      Tok_LParen::(tok (pos+1) str)
      
    (*Operators*)
    else if (Str.string_match re_add str pos) then 
      Tok_Add::(tok (pos+1) str)

    else if (Str.string_match re_sub str pos) then 
      Tok_Sub::(tok (pos+1) str)

    else if (Str.string_match re_mult str pos) then 
      Tok_Mult::(tok (pos+1) str)

    else if (Str.string_match re_div str pos) then 
      Tok_Div::(tok (pos+1) str)

    else if (Str.string_match re_pow str pos) then 
      Tok_Pow::(tok (pos+1) str)

    (*Comparisions*)
    else if (Str.string_match re_greaterequal str pos) then 
      Tok_GreaterEqual::(tok (pos+2) str)

    else if (Str.string_match re_greater str pos) then 
      Tok_Greater::(tok (pos+1) str)

    else if (Str.string_match re_or str pos) then 
      Tok_Or::(tok (pos+2) str)

    else if (Str.string_match re_equal str pos) then 
      Tok_Equal::(tok (pos+2) str)

    else if (Str.string_match re_notequal str pos) then 
      Tok_NotEqual::(tok (pos+2) str)

    else if (Str.string_match re_not str pos) then 
      Tok_Not::(tok (pos+1) str)

    else if (Str.string_match re_less str pos) then 
      Tok_Less::(tok (pos+1) str)

    else if (Str.string_match re_lessequal str pos) then 
      Tok_LessEqual::(tok (pos+2) str) 

    else if (Str.string_match re_and str pos) then 
      Tok_And::(tok (pos+2) str)

    else if (Str.string_match re_assign str pos) then 
      Tok_Assign::(tok (pos+1) str)

    (*keywords*)
    else if (Str.string_match re_main str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_Main pos str

    else if (Str.string_match re_print str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_Print pos str

    else if (Str.string_match re_for str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_For pos str

    else if (Str.string_match re_while str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_While pos str

    else if (Str.string_match re_if str pos) then 
      let keyword = Str.matched_string str in
      tok_helper keyword Tok_If pos str

    else if (Str.string_match re_else str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_Else pos str

    else if (Str.string_match re_int_type str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_Int_Type pos str

    else if (Str.string_match re_bool_type str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_Bool_Type pos str

    else if (Str.string_match re_from str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_From pos str

    else if (Str.string_match re_to str pos) then 
      let keyword = Str.matched_string str in 
      tok_helper keyword Tok_To pos str

    (*lastly, id*)
    else if (Str.string_match re_id str pos) then 
      let matched = Str.matched_string str in 
      (Tok_ID matched)::(tok (pos + (String.length matched)) str)
    else 
      failwith "Wrong Symbol"
  in 
  tok 0 input
;;
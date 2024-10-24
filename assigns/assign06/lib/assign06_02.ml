open Utils
let parse (tokens: tok list) : expr option =
    let rec parse_rpn toks expr_stack =
        match toks, expr_stack with
        | [], [final_expr] -> Some final_expr
        | [], _ -> None
        | TNum n :: rest, _ -> 
                parse_rpn rest (Num n :: expr_stack)
        | TAdd :: rest, e2 :: e1 :: stack_tail -> 
                parse_rpn rest (Add (e1, e2) :: stack_tail)
        | TLt :: rest, e2 :: e1 :: stack_tail -> 
                parse_rpn rest (Lt (e1, e2) :: stack_tail)
        | TIte :: rest, e3 :: e2 :: e1 :: stack_tail -> 
                parse_rpn rest (Ite (e1, e2, e3) :: stack_tail)
        | _ -> None
    in
    parse_rpn tokens []
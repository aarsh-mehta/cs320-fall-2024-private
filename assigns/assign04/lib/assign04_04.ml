type ident = string

type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = 
  | Int
  | Bool

type context = (ident * ty') list

let rec lookup ctx x =
  match ctx with
  | [] -> None
  | (y, ty) :: rest -> if x = y then Some ty else lookup rest x

let rec type_of' gamma e =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Var x -> lookup gamma x
  | Add (e1, e2) ->
      (match type_of' gamma e1, type_of' gamma e2 with
       | Some Int, Some Int -> Some Int
       | _ -> None)
  | Or (e1, e2) ->
      (match type_of' gamma e1, type_of' gamma e2 with
       | Some Bool, Some Bool -> Some Bool
       | _ -> None)
  | IfThenElse (e1, e2, e3) ->
      (match type_of' gamma e1 with
       | Some Bool ->
           (match type_of' gamma e2, type_of' gamma e3 with
            | Some t2, Some t3 when t2 = t3 -> Some t2
            | _ -> None)
       | _ -> None)
  | Let (x, e1, e2) ->
      (match type_of' gamma e1 with
       | Some t1 ->
           let gamma' = (x, t1) :: gamma in
           type_of' gamma' e2
       | None -> None)

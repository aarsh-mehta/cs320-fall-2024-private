type ident = string

type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec find_type (gamma : ctxt) (x : ident) : ty option =
  match gamma with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else find_type rest x

let rec type_of (gamma : ctxt) (e : expr) : ty option =
  match e with
  | Var x -> find_type gamma x
  | Fun (x, t1, body) ->
      let extended_gamma = (x, t1) :: gamma in
      (match type_of extended_gamma body with
       | Some t2 -> Some (Arr (t1, t2))
       | None -> None)
  | App (e1, e2) ->
      let t1_opt = type_of gamma e1 in
      let t2_opt = type_of gamma e2 in
      (match t1_opt, t2_opt with
       | Some (Arr (t2, t)), Some t2' when t2 = t2' -> Some t
       | _ -> None)
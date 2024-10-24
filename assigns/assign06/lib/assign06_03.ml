open Utils
let rec type_of (e: expr) : ty option =
  let check_binary_op e1 e2 expected_ty result_ty =
    match (type_of e1, type_of e2) with
    | (Some t1, Some t2) when t1 = expected_ty && t2 = expected_ty -> Some result_ty
    | _ -> None
  in
  match e with
  | Num _ -> Some TInt  
  | Add (e1, e2) -> check_binary_op e1 e2 TInt TInt 
  | Lt (e1, e2) -> check_binary_op e1 e2 TInt TBool  
  | Ite (e1, e2, e3) -> (
      match type_of e1 with
      | Some TBool -> (
          match (type_of e2, type_of e3) with
          | (Some t2, Some t3) when t2 = t3 -> Some t2 
          | _ -> None
        )
      | _ -> None
    )

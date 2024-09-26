let group (l : int list) : int list list option =
  let rec aux acc current_group prev = function
    | [] -> 
        if current_group = [] then Some (List.rev acc)
        else Some (List.rev (List.rev current_group :: acc))
    | 0 :: rest ->
        if current_group = [] then None
        else 
          (match rest with
          | [] -> None
          | x :: _ -> 
              if x * prev > 0 then None
              else aux (List.rev current_group :: acc) [] 0 rest)
    | x :: rest ->
        match current_group with
        | [] -> aux acc [x] x rest
        | y :: _ ->
            if prev = 0 && x * y <= 0 then
              aux acc (x :: current_group) x rest
            else if prev <> 0 && x * y > 0 then
              aux acc (x :: current_group) x rest
            else None
  in
  match l with
  | [] -> Some []
  | 0 :: _ -> None
  | x :: rest -> aux [] [x] x rest
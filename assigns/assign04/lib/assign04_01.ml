let lifespan f s p =
  let rec aux s steps visited =
    if p s then Some steps
    else if List.mem s visited then None
    else aux (f s) (steps + 1) (s :: visited)
  in
  aux s 0 []

let compare_lifespans a b =
  match a, b with
  | None, None -> 0
  | None, Some _ -> 1
  | Some _, None -> -1
  | Some n, Some m -> compare n m

let last_function_standing funcs start pred =
  let rec helper fs max_lifespan best_funcs =
    match fs with
    | [] ->
        (match best_funcs with
         | [f] -> Some f
         | _ -> None)
    | f :: rest ->
        let lifespan_f = lifespan f start pred in
        let comp = compare_lifespans lifespan_f max_lifespan in
        if comp > 0 then
          helper rest lifespan_f [f]
        else if comp = 0 then
          helper rest max_lifespan (f :: best_funcs)
        else
          helper rest max_lifespan best_funcs
  in
  helper funcs (Some (-1)) []

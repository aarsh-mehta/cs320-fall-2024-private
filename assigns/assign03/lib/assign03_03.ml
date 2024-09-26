type tree =
  | Leaf of int
  | Node of tree list

let rec collapse h t =
  if h = 1 then
    match t with
    | Leaf _ as leaf -> leaf
    | Node children ->
       let rec collect = function
         | [] -> []
         | Leaf x :: cs -> Leaf x :: collect cs
         | Node [] :: cs -> Node [] :: collect cs
         | Node cs' :: cs -> collect cs' @ collect cs
       in Node (collect children)
  else
    match t with
    | Leaf _ as leaf -> leaf
    | Node children ->
       let children' = List.map (collapse (h - 1)) children in
       Node children'
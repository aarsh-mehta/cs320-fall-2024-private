type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let sum_tr t =
  let rec aux stack acc =
    match stack with
    | [] -> acc
    | Leaf :: rest ->
        aux rest acc
    | Node (x, l, r) :: rest ->
        aux (l :: r :: rest) (acc + x)
  in
  aux [t] 0
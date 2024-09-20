type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let mk_matrix (entries_list : float list) ((row, column) : int * int) : matrix =
  let rec lot l n =
    match l with
    | [] -> []
    | _ ->
        let rec take m acc lst =
          if m = 0 then (List.rev acc, lst)
          else match lst 
        with
            | [] -> (List.rev acc, [])
            | h :: t -> take (m - 1) (h :: acc) t
        in let (l, r) = take column [] l 
      in l :: lot r n
      in let entries = lot entries_list column 
in { entries; rows = row; cols = column }
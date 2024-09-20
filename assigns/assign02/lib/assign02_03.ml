type dir = 
  | North 
  | South 
  | East 
  | West

type path = dir list

    let dist (dirs : path) : float =
      let rec calc_xy dirs x y =
        match dirs with
        | [] -> (x, y)
        | start :: ending ->
            match start with
            | North -> calc_xy ending x (y+1)
            | South -> calc_xy ending x (y-1)
            | East -> calc_xy ending (x+1) y
            | West -> calc_xy ending (x-1) y
        in
      let (endofx, endofy) = calc_xy dirs 0 0 in
      sqrt (float_of_int (endofx * endofx + endofy * endofy))



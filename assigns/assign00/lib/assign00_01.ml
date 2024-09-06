let sqrt n =
  let rec calc k =
    if k * k >= n 
      then k
    else calc (k + 1)
  in calc 0

let is_prime n =
  let rec go i =
    if i * i > n 
      then true
    else if n mod i = 0 
      then false
    else
      go (i + 1)
  in
  if n < 2
     then false
  else go 2

let nth_prime n =
  let rec search count current =
    if is_prime current 
      then
      if count = n then current else search (count + 1) (current + 1)
    else
      search count (current + 1)
  in
  search 0 2
open Assign01_02

let nth s i =
  let rec sequence s l =
    if s mod l <> 0 
      then 0
    else 1 + sequence (s / l) l
  in
  let l = nth_prime i in
  sequence s l
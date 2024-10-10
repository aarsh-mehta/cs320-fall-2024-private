type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module ListSet = struct
  type t = int list

  let empty = []

  let singleton x = [x]

  let rec mem x s = 
    match s with
    | [] -> false
    | h :: t -> if x = h then true else mem x t

  let union s1 s2 =
    let rec merge l1 l2 =
      match l1, l2 with
      | [], l | l, [] -> l
      | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then h1 :: merge t1 t2
        else if h1 < h2 then h1 :: merge t1 l2
        else h2 :: merge l1 t2
    in
    merge s1 s2

  let card s = 
    let rec count acc lst =
      match lst with
      | [] -> acc
      | _ :: t -> count (acc + 1) t
    in
    count 0 s
end

module FuncSet = struct
  type t = set_info

  let empty = {
    ind = (fun _ -> false);
    mn = 1; 
    mx = 0;
  }

  let singleton x = {
    ind = (fun y -> y = x);
    mn = x;
    mx = x;
  }

  let mem x s = s.ind x

  let union s1 s2 =
    let is_empty s = s.mn > s.mx in
    if is_empty s1 && is_empty s2 then empty
    else if is_empty s1 then s2
    else if is_empty s2 then s1
    else {
      ind = (fun x -> s1.ind x || s2.ind x);
      mn = min s1.mn s2.mn;
      mx = max s1.mx s2.mx;
    }

  let card s =
    if s.mn > s.mx then 0
    else
      let rec count acc x =
        if x > s.mx then acc
        else if s.ind x then count (acc + 1) (x + 1)
        else count acc (x + 1)
      in
      count 0 s.mn
end

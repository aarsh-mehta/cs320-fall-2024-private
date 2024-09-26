(* mk_unique_keys: (string * int) list -> (string * int) list *)
let mk_unique_keys alst =
  (* Helper function to add key-value pairs to the accumulator list, summing duplicates *)
  let add_to_acc key value acc =
    match List.assoc_opt key acc with
    | None -> (key, value) :: acc
    | Some _ -> 
      List.map (fun (k, v) -> if k = key then (k, v + value) else (k, v)) acc
  in
  (* Main function to process the input list *)
  let rec process_list lst acc =
    match lst with
    | [] -> acc
    | (key, value) :: tail -> process_list tail (add_to_acc key value acc)
  in
  process_list alst []

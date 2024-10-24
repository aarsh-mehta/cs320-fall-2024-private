open Utils
let lex (s: string) : tok list option =
  let words = split s in  (* Split the string into a list of words *)
  
  (* Tail-recursive function to process the tokens *)
  let process_words ws =
    List.fold_left (fun acc w ->
      match acc with
      | None -> None  (* If we already encountered an error, propagate it *)
      | Some toks -> (
          match tok_of_string_opt w with
          | Some tok -> Some (tok :: toks)  (* Add the token to the list *)
          | None -> None  (* If the word fails to convert, return None *)
        )
    ) (Some []) ws  (* Initial accumulator is Some [], meaning an empty list of tokens *)
  in
  
  (* Call the tail-recursive function and reverse the final result to maintain order *)
  match process_words words with
  | Some toks -> Some (List.rev toks)  (* Reverse the list to keep the original order *)
  | None -> None  (* If processing fails, return None *)
(** Filters and evaluates a valid mul expression from the input expr. *)
let filter expr =
    (* Convert to char list. *)
    match String.to_seq expr |> List.of_seq with
    (* If does not begin with '(', return 0 for invalid expression. *)
    | '(' :: vals -> (
        (* If does not match to inner :: _ :: _, there is no inner part of parentheses. *)
        match String.split_on_char ')' (String.of_seq (List.to_seq vals)) with
        | inner :: _ :: _ ->
            (* If the inner part of parentheses are 2 elements split by ',' that are valid itegers, return the product. *)
            (match String.split_on_char ',' inner with
            | [l; r] when (try let _ = int_of_string l in let _ = int_of_string r in true with _ -> false) -> 
                    (int_of_string l)*(int_of_string r)
            | _ -> 0
            )
        | _  -> 0
    )
    | _ -> 0

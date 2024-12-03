let filter expr =
    match String.to_seq expr |> List.of_seq with
    | '(' :: vals -> (
        match String.split_on_char ')' (String.of_seq (List.to_seq vals)) with
        | inner :: _ :: _ ->
            (match String.split_on_char ',' inner with
            | [l; r] when (try let _ = int_of_string l in let _ = int_of_string r in true with _ -> false) -> 
                    (int_of_string l)*(int_of_string r)
            | _ -> 0
            )
        | _  -> 0
    )
    | _ -> 0

let solve filename =
    Input_reader.read_input filename
    |> List.hd
    |> Str.split (Str.regexp "mul")
    |> List.map (fun expr -> filter expr)
    |> List.fold_left (+) 0

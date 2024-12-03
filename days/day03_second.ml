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

let take_enabled (str : string) =
    let rec aux (inp : string) (acc : string) (tacc : string) (enabled : bool) =
        match tacc with
        | "do()" -> aux inp acc "" true
        | "don't()" -> aux inp acc "" false
        | _ -> (
            match List.of_seq (String.to_seq inp) with
            | [] -> acc
            | a :: tail -> 
            let
                tacc =
                if
                    String.starts_with ~prefix:(tacc ^ (String.make 1 a)) "do()" 
                    || String.starts_with ~prefix:(tacc ^ (String.make 1 a)) "don't()" 
                then
                    (tacc ^ (String.make 1 a))
                else
                    ""
            in
            if enabled then
                aux (tail |> List.to_seq |> String.of_seq) (acc ^ (String.make 1 a)) tacc enabled
            else
                aux (tail |> List.to_seq |> String.of_seq) acc tacc enabled
        )


    in aux str "" "" true


let solve filename =
    Input_reader.read_input filename
    |> List.hd
    |> take_enabled
    |> Str.split (Str.regexp "mul")
    |> List.map (fun expr -> filter expr)
    |> List.fold_left (+) 0

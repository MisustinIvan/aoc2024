(**
    Solves the puzzle for day 7 part 2, loading input from given filename.
*)
let solve filename = filename
    |> Input_reader.read_input
    |> List.map (fun ln -> ln
        |> Str.split (Str.regexp ": ")
        |> (function
            | [head; tail] -> (
                int_of_string head,
                tail
                |> String.split_on_char ' '
                |> List.map int_of_string
            )
            | _ -> failwith "Expected calibration result and numbers"
        )
    )
    |> List.map (fun (expected, vals) ->
            let rec aux vals = 
                match vals with
                | [n] -> expected = n
                | a :: b :: tail -> if aux ((a+b) :: tail) then true
                                    else if aux ((a*b) :: tail) then true
                                    else aux ((int_of_string (string_of_int a ^ string_of_int b)) :: tail)
                | [] -> false
            in
            if aux vals then expected else 0
    )
    |> List.fold_left (+) 0


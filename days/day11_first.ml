open Day11_shared

(**
    Solves the puzzle for day 11 part 1, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    (* Process into workable format. *)
    |> (function
        | [head] -> head
        | _ -> failwith "expected a single line")
    |> String.split_on_char ' '
    |> List.map int_of_string
    |> fun sts ->
            let rec aux sts n =
                match n with
                | 0 -> sts
                | n -> aux (List.flatten (List.map update sts)) (n-1)
            in
            aux sts 25
    |> List.length


(** Updates a given stone according to the rules described in the puzzle. *)
let update num =
    match num with
    | 0 -> [1]
    | _ when ((num |> string_of_int |> String.length) mod 2) = 0 -> 
            let s  = num |> string_of_int in
            let mid = (s |> String.length) /2 in
            [int_of_string (String.sub s 0 mid); int_of_string (String.sub s mid mid)]
    | _ -> [num*2024]

(**
    Solves the puzzle for day 10 part 1, loading input from given filename.
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


(**
    Solves the puzzle for day 1 part 2, loading input from given filename.
*)
let solve filename =
    (* read split lines from file *)
    Input_reader.read_input filename
    (* split and convert to list list int *)
    |> List.map (
        fun line -> String.split_on_char ' ' line
        |> List.filter (fun s -> s <> "")
        |> List.map int_of_string)
    (* convert to list (int * int) *)
    |> List.map (
        function
        | [l; r] -> (l, r)
        | _ -> failwith "expected only two integers")
    (* convert to (list int * list int) *)
    |> List.fold_left
        (fun (llist, rlist) (l, r) -> (l :: llist, r :: rlist))
        ([], [])
    (* count the number of occurences of elements in each list *)
    |> fun (llist, rlist) ->
            let occurs lst =
                List.fold_left (fun acc x ->
                    let count = try List.assoc x acc with Not_found -> 0 in
                    (x, count + 1) :: (List.remove_assoc x acc)
                ) [] lst
            in
            (occurs llist, occurs rlist)
    |> fun (loccurs, roccurs) ->
        List.map (fun (k, v) -> k * v * try List.assoc k roccurs with Not_found -> 0) loccurs
    |> List.fold_left (fun acc x -> acc + x) 0

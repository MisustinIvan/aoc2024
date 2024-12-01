let solve () =
    (* read split lines from file *)
    Input_reader.read_input "./inputs/day1.txt"
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
    (* sort the lists *)
    |> fun (llist, rlist) -> (List.sort compare llist, List.sort compare rlist)
    (* convert them to list int of differences *)
    |> fun (llist, rlist) -> List.map2 (fun l r -> abs (l - r)) llist rlist
    (* sum up the differences *)
    |> fun distances -> List.fold_left (fun acc d -> acc + d) 0 distances
    |> fun sum -> print_endline (string_of_int sum)

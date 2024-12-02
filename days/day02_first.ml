open Day02_shared

(**
    Solves the puzzle for day 2 part 1, loading input from given filename.
*)
let solve filename =
    Input_reader.read_input filename
    (* split into individual strings and convert them to ints *)
    |> List.map (fun ln -> List.map int_of_string (String.split_on_char ' ' ln))
    (* convert the reports into differences *)
    |> List.map (fun el -> diffs el)
    (* filter the differences and count them up *)
    |> List.fold_left (fun acc dl -> acc + if is_safe dl then 1 else 0) 0

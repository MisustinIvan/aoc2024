open Day06_shared

(**
    Solves the puzzle for day 6 part 1, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    (* Convert to usable format. *)
    |> List.map (fun ln -> ln |> String.to_seq |> Array.of_seq)
    (* Extract the position of the guard. *)
    |> fun table -> (Array.of_list table, find_guard table)
    (* Walk through the table, marking all the visited fields. *)
    |> walk_table

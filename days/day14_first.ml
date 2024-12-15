open Day14_shared

(**
    Solves the puzzle for day 14 part 1, loading input from given filename.
*)
let solve filename = filename
    |> Input_reader.read_input
    |> List.filter_map (
        fun ln ->
            let regex = Str.regexp "p=\\([0-9-]+\\),\\([0-9-]+\\) v=\\([0-9-]+\\),\\([0-9-]+\\)" in
            if Str.string_match regex ln 0
            then
                Some (((int_of_string (Str.matched_group 1 ln)), (int_of_string (Str.matched_group 2 ln))),
                    ((int_of_string (Str.matched_group 3 ln)), (int_of_string (Str.matched_group 4 ln))))
            else
                None
        )
    |> List.map (fun specs -> iter_bot specs (101, 103) 100)
    |> safety

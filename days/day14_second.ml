open Day14_shared

(**
    Solves the puzzle for day 14 part 2, loading input from given filename.
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
    |> fun els ->
            (* if it's supposed to make a christmas tree, i would assume that there
               would be some extremes, that means min an max. so we search for the
               lowest safety, as ideally it should be about 0. (if we 0 robots in a quadrant)*)
            let max_iter = 100000 in
            let rec aux acc i =
                match i with
                | 0 -> acc
                | _ -> aux ((i, safety (List.map (fun el -> iter_bot el (101, 103) i) els)) :: acc) (i-1)
            in
            let (vals : (int * int) list) = aux [] max_iter in
            let (miter, msafety) = List.fold_left (fun (ia,sa) (i,s) -> if s < sa then (i,s) else (ia,sa)) (0,max_int) vals in
            Printf.printf "MITER: %d; MSAFETY: %d\n" miter msafety;
            miter

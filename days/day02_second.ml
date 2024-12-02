open Day02_shared
open Util

(**
    Solves the puzzle for day 2 part 2, loading input from given filename.
*)
let solve filename =
    Input_reader.read_input filename
    (* split into individual strings and convert them to ints *)
    |> List.map (fun ln -> List.map int_of_string (String.split_on_char ' ' ln))
    |> List.map (fun report ->
        (* check if safe right away *)
        let differences = diffs report in
        if is_safe differences then 1
        else
            let len = List.length report in
            (* go through the list and see if it becomes safe by removing a single element *)
            let rec try_fix i =
                if i >= len then 0
                else
                    let fixed_report = rm_el report i in
                    let fixed_diffs = diffs fixed_report in
                    if is_safe fixed_diffs then 1
                    else try_fix (i+1)
                in
            try_fix 0
    )
    (* count the number of safe reports *)
    |> List.fold_left (+) 0

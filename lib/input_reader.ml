let read_input fn =
    let ic = open_in fn in
    let rec af acc =
        try
            let ln = input_line ic in
            af (ln :: acc)
        with
        | End_of_file ->
                close_in ic;
                List.rev acc
    in
    af []

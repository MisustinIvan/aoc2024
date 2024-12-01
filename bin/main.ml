let () =
    let day = Sys.argv.(1) in
    match day with
    | "1_0" -> Days.Day01_first.solve ()
    | "1_1" -> Days.Day01_second.solve ()
    | _ -> print_endline "Invalid day, usage: aoc <day_number>_<0|1>"

let () =
    let day = Sys.argv.(1) in
    match day with
    | "1" -> Days.Day01.solve ()
    | _ -> print_endline "Invalid day, usage: aoc <day_number>"

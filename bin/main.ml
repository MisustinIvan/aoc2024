let () =
    let day = Sys.argv.(1) in
    match day with
    | "1_0" -> Days.Day01_first.solve "./inputs/day1.txt" |> print_int
    | "1_1" -> Days.Day01_second.solve "./inputs/day1.txt" |> print_int
    | "2_0" -> Days.Day02_first.solve "./inputs/day2.txt" |> print_int
    | "2_1" -> Days.Day02_second.solve "./inputs/day2.txt" |> print_int
    | _ -> print_endline "Invalid day, usage: aoc <day_number>_<0|1>"

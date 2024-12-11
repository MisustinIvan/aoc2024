let () =
    let day = Sys.argv.(1) in
    match day with
    | "1_0" -> Days.Day01_first.solve "./inputs/day1.txt" |> print_int
    | "1_1" -> Days.Day01_second.solve "./inputs/day1.txt" |> print_int
    | "2_0" -> Days.Day02_first.solve "./inputs/day2.txt" |> print_int
    | "2_1" -> Days.Day02_second.solve "./inputs/day2.txt" |> print_int
    | "3_0" -> Days.Day03_first.solve "./inputs/day3.txt" |> print_int
    | "3_1" -> Days.Day03_second.solve "./inputs/day3.txt" |> print_int
    | "4_0" -> Days.Day04_first.solve "./inputs/day4.txt" |> print_int
    | "4_1" -> Days.Day04_second.solve "./inputs/day4.txt" |> print_int
    | "5_0" -> Days.Day05_first.solve "./inputs/day5.txt" |> print_int
    | "5_1" -> Days.Day05_second.solve "./inputs/day5.txt" |> print_int
    | "6_0" -> Days.Day06_first.solve "./inputs/day6.txt" |> print_int
    | "6_1" -> Days.Day06_second.solve "./inputs/day6.txt" |> print_int
    | "7_0" -> Days.Day07_first.solve "./inputs/day7.txt" |> print_int
    | "7_1" -> Days.Day07_second.solve "./inputs/day7.txt" |> print_int
    | "8_0" -> Days.Day08_first.solve "./inputs/day8.txt" |> print_int
    | "8_1" -> Days.Day08_second.solve "./inputs/day8.txt" |> print_int
    | "9_0" -> Days.Day09_first.solve "./inputs/day9.txt" |> print_int
    | "9_1" -> Days.Day09_second.solve "./inputs/day9.txt" |> print_int
    | "10_0" -> Days.Day10_first.solve "./inputs/day10.txt" |> print_int
    | "10_1" -> Days.Day10_second.solve "./inputs/day10.txt" |> print_int
    | "11_0" -> Days.Day11_first.solve "./inputs/day11.txt" |> print_int
    | "11_1" -> Days.Day11_second.solve "./inputs/day11.txt" |> print_int
    | _ -> print_endline "Invalid day, usage: aoc <day_number>_<0|1>"

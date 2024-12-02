open OUnit2

let test_solve_day01_first _ =
    let solution = Days.Day01_first.solve "../inputs/day1_test.txt" in
    let expected_result = 11 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day01_second _ =
    let solution = Days.Day01_second.solve "../inputs/day1_test.txt" in
    let expected_result = 31 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 01 Tests" >:::
        [
            "test_solve_day01_first" >:: test_solve_day01_first;
            "test_solve_day01_second" >:: test_solve_day01_second;
        ]

let _ = run_test_tt_main suite

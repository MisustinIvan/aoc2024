open OUnit2

let test_solve_day04_first _ =
    let solution = Days.Day04_first.solve "../inputs/day4_test.txt" in
    let expected_result = 18 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day04_second _ =
    let solution = Days.Day04_second.solve "../inputs/day4_test.txt" in
    let expected_result = 9 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 04 Tests" >:::
        [
            "test_solve_day04_first" >:: test_solve_day04_first;
            "test_solve_day04_second" >:: test_solve_day04_second;
        ]

let _ = run_test_tt_main suite

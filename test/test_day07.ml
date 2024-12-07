open OUnit2

let test_solve_day07_first _ =
    let solution = Days.Day07_first.solve "../inputs/day7_test.txt" in
    let expected_result = 3749 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day07_second _ =
    let solution = Days.Day07_second.solve "../inputs/day7_test.txt" in
    let expected_result = 11387 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 07 Tests" >:::
        [
            "test_solve_day07_first" >:: test_solve_day07_first;
            "test_solve_day07_second" >:: test_solve_day07_second;
        ]

let _ = run_test_tt_main suite

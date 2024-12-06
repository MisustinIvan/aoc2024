open OUnit2

let test_solve_day06_first _ =
    let solution = Days.Day06_first.solve "../inputs/day6_test.txt" in
    let expected_result = 41 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day06_second _ =
    let solution = Days.Day06_second.solve "../inputs/day6_test.txt" in
    let expected_result = 6 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 06 Tests" >:::
        [
            "test_solve_day06_first" >:: test_solve_day06_first;
            "test_solve_day06_second" >:: test_solve_day06_second;
        ]

let _ = run_test_tt_main suite

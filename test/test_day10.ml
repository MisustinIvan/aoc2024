open OUnit2

let test_solve_day10_first _ =
    let solution = Days.Day10_first.solve "../inputs/day10_test.txt" in
    let expected_result = 36 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day10_second _ =
    let solution = Days.Day10_second.solve "../inputs/day10_test.txt" in
    let expected_result = 81 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 10 Tests" >:::
        [
            "test_solve_day10_first" >:: test_solve_day10_first;
            "test_solve_day10_second" >:: test_solve_day10_second;
        ]

let _ = run_test_tt_main suite

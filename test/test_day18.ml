open OUnit2

let test_solve_day18_first _ =
    let solution = Days.Day18_first.solve "../inputs/day18_test.txt" in
    let expected_result = 146 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day18_second _ =
    let solution = Days.Day18_second.solve "../inputs/day18_test.txt" in
    let expected_result = "NONE" in
    assert_equal ~printer:Fun.id
    expected_result solution

let suite =
    "Day 18 Tests" >:::
        [
            "test_solve_day18_first" >:: test_solve_day18_first;
            "test_solve_day18_second" >:: test_solve_day18_second;
        ]

let _ = run_test_tt_main suite

open OUnit2

let test_solve_day19_first _ =
    let solution = Days.Day19_first.solve "../inputs/day19_test.txt" in
    let expected_result = 6 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day19_second _ =
    let solution = Days.Day19_second.solve "../inputs/day19_test.txt" in
    let expected_result = 16 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 19 Tests" >:::
        [
            "test_solve_day19_first" >:: test_solve_day19_first;
            "test_solve_day19_second" >:: test_solve_day19_second;
        ]

let _ = run_test_tt_main suite

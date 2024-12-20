open OUnit2

let test_solve_day09_first _ =
    let solution = Days.Day09_first.solve "../inputs/day9_test.txt" in
    let expected_result = 1928 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day09_second _ =
    let solution = Days.Day09_second.solve "../inputs/day9_test.txt" in
    let expected_result = 2858 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 09 Tests" >:::
        [
            "test_solve_day09_first" >:: test_solve_day09_first;
            "test_solve_day09_second" >:: test_solve_day09_second;
        ]

let _ = run_test_tt_main suite

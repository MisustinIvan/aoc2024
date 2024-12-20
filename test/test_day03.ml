open OUnit2

let test_solve_day03_first _ =
    let solution = Days.Day03_first.solve "../inputs/day3_test.txt" in
    let expected_result = 161 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day03_second _ =
    let solution = Days.Day03_second.solve "../inputs/day3_test.txt" in
    let expected_result = 48 in
    assert_equal ~printer:string_of_int
    expected_result solution



let suite =
    "Day 03 Tests" >:::
        [
            "test_solve_day03_first" >:: test_solve_day03_first;
            "test_solve_day03_second" >:: test_solve_day03_second;
        ]

let _ = run_test_tt_main suite

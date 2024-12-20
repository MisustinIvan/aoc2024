open OUnit2

let test_solve_day08_first _ =
    let solution = Days.Day08_first.solve "../inputs/day8_test.txt" in
    let expected_result = 14 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day08_second _ =
    let solution = Days.Day08_second.solve "../inputs/day8_test.txt" in
    let expected_result = 34 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 08 Tests" >:::
        [
            "test_solve_day08_first" >:: test_solve_day08_first;
            "test_solve_day08_second" >:: test_solve_day08_second;
        ]

let _ = run_test_tt_main suite

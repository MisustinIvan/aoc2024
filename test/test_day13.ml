open OUnit2

let test_solve_day13_first _ =
    let solution = Days.Day13_first.solve "../inputs/day13_test.txt" in
    let expected_result = 480 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 13 Tests" >:::
        [
            "test_solve_day13_first" >:: test_solve_day13_first;
        ]

let _ = run_test_tt_main suite

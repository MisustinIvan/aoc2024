open OUnit2

let test_solve_day14_first _ =
    let solution = Days.Day14_first.solve "../inputs/day14_test.txt" in
    let expected_result = 12 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 14 Tests" >:::
        [
            "test_solve_day14_first" >:: test_solve_day14_first;
        ]

let _ = run_test_tt_main suite

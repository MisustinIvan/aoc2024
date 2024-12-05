open OUnit2

let test_solve_day05_first _ =
    let solution = Days.Day05_first.solve "../inputs/day5_test.txt" in
    let expected_result = 143 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 05 Tests" >:::
        [
            "test_solve_day05_first" >:: test_solve_day05_first;
        ]

let _ = run_test_tt_main suite

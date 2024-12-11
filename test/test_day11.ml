open OUnit2

let test_solve_day11_first _ =
    let solution = Days.Day11_first.solve "../inputs/day11_test.txt" in
    let expected_result = 55312 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 11 Tests" >:::
        [
            "test_solve_day11_first" >:: test_solve_day11_first;
        ]

let _ = run_test_tt_main suite

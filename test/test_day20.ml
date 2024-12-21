open OUnit2

let test_solve_day20_first _ =
    let solution = Days.Day20_first.solve "../inputs/day20_test.txt" in
    let expected_result = 0 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day20_second _ =
    let solution = Days.Day20_second.solve "../inputs/day20_test.txt" in
    let expected_result = 0 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 20 Tests" >:::
        [
            "test_solve_day20_first" >:: test_solve_day20_first;
            "test_solve_day20_second" >:: test_solve_day20_second;
        ]

let _ = run_test_tt_main suite

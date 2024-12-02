open OUnit2

let test_solve_day02_first _ =
    let solution = Days.Day02_first.solve "../inputs/day2_test.txt" in
    let expected_result = 2 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day02_second _ =
    let solution = Days.Day02_second.solve "../inputs/day2_test.txt" in
    let expected_result = 4 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day02_second_edge_cases _ =
    let solution = Days.Day02_second.solve "../inputs/day2_edge_cases_test.txt" in
    let expected_result = 12 in
    assert_equal ~printer:string_of_int
    expected_result solution


let suite =
    "Day 02 Tests" >:::
        [
            "test_solve_day02_first" >:: test_solve_day02_first;
            "test_solve_day02_second" >:: test_solve_day02_second;
            "test_solve_day02_edge_cases" >:: test_solve_day02_second_edge_cases;
        ]

let _ = run_test_tt_main suite

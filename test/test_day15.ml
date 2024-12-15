open OUnit2

let test_solve_day15_first_0 _ =
    let solution = Days.Day15_first.solve "../inputs/day15_test_0.txt" in
    let expected_result = 2028 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day15_first_1 _ =
    let solution = Days.Day15_first.solve "../inputs/day15_test_1.txt" in
    let expected_result = 10092 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day15_second _ =
    let solution = Days.Day15_second.solve "../inputs/day15_test_1.txt" in
    let expected_result = 9021 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 15 Tests" >:::
        [
            "test_solve_day15_first_0" >:: test_solve_day15_first_0;
            "test_solve_day15_first_1" >:: test_solve_day15_first_1;
            "test_solve_day15_second" >:: test_solve_day15_second;
        ]

let _ = run_test_tt_main suite

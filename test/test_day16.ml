open OUnit2

let test_solve_day16_first_0 _ =
    let solution = Days.Day16_first.solve "../inputs/day16_test_0.txt" in
    let expected_result = 7036 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day16_first_1 _ =
    let solution = Days.Day16_first.solve "../inputs/day16_test_1.txt" in
    let expected_result = 11048 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day16_second_0 _ =
    let solution = Days.Day16_second.solve "../inputs/day16_test_0.txt" in
    let expected_result = 45 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day16_second_1 _ =
    let solution = Days.Day16_second.solve "../inputs/day16_test_1.txt" in
    let expected_result = 64 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 16 Tests" >:::
        [
            "test_solve_day16_first_0" >:: test_solve_day16_first_0;
            "test_solve_day16_first_1" >:: test_solve_day16_first_1;
            "test_solve_day16_second_0" >:: test_solve_day16_second_0;
            "test_solve_day16_second_1" >:: test_solve_day16_second_1;
        ]

let _ = run_test_tt_main suite

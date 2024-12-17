open OUnit2

let test_solve_day17_first _ =
    let solution = Days.Day17_first.solve "../inputs/day17_test_0.txt" in
    let expected_result = "4,6,3,5,6,3,5,2,1,0" in
    assert_equal ~printer:Fun.id
    expected_result solution

let test_solve_day17_second _ =
    let solution = Days.Day17_second.solve "../inputs/day17_test_1.txt" in
    let expected_result = 117440 in
    assert_equal ~printer:string_of_int
    expected_result solution

let suite =
    "Day 17 Tests" >:::
        [
            "test_solve_day17_first" >:: test_solve_day17_first;
            "test_solve_day17_second" >:: test_solve_day17_second;
        ]

let _ = run_test_tt_main suite

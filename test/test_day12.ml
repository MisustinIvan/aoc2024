open OUnit2

let test_solve_day12_first _ =
    let solution = Days.Day12_first.solve "../inputs/day12_test.txt" in
    let expected_result = 1930 in
    assert_equal ~printer:string_of_int
    expected_result solution

let test_solve_day12_second _ =
    let solution = Days.Day12_second.solve "../inputs/day12_test.txt" in
    let expected_result = 1206 in
    assert_equal ~printer:string_of_int
    expected_result solution



let suite =
    "Day 12 Tests" >:::
        [
            "test_solve_day12_first" >:: test_solve_day12_first;
            "test_solve_day12_second" >:: test_solve_day12_second;
        ]

let _ = run_test_tt_main suite

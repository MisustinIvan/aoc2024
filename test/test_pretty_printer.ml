open OUnit2
open Pretty_printer

let test_string_of_int_list _ =
    let input = [1; 2; 3; 4; 5; 6] in
    let expeced = "[1; 2; 3; 4; 5; 6]" in
    let result = string_of_int_list input in
    assert_equal ~printer:(fun s -> s)
    result expeced

let test_string_of_string_list _ =
    let input = ["ligma"; "balls"] in
    let expeced = "[ligma; balls]" in
    let result = string_of_string_list input in
    assert_equal ~printer:(fun s -> s)
    result expeced

let suite =
    "Pretty printer tests" >:::
        [
            "test_string_of_int_list" >:: test_string_of_int_list;
            "test_string_of_string_list" >:: test_string_of_string_list;
        ]

let _ = run_test_tt_main suite

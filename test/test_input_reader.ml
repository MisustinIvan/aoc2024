open OUnit2
open Input_reader
open Pretty_printer

let test_read _ =
    let result = read_input "../inputs/test_input.txt" in
    let expected_result = ["ligma"; "balls"] in
    assert_equal ~printer:string_of_string_list
    result expected_result


let suite =
    "Input reader tests" >:::
        [
            "test_read" >:: test_read;
        ]

let _ = run_test_tt_main suite

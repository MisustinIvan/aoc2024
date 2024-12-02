open OUnit2
open Pretty_printer
open Util

let test_rm_el_middle _ =
    let input = [1;2;3;4;5;6] in
    let index = 2 in
    let expeced = [1;2;4;5;6] in
    let result = rm_el input index in
    assert_equal ~printer:string_of_int_list
    result expeced

let test_rm_el_first _ =
    let input = [1;2;3;4;5;6] in
    let index = 0 in
    let expeced = [2;3;4;5;6] in
    let result = rm_el input index in
    assert_equal ~printer:string_of_int_list
    result expeced

let test_rm_el_last _ =
    let input = [1;2;3;4;5;6] in
    let index = (List.length input)-1 in
    let expeced = [1;2;3;4;5] in
    let result = rm_el input index in
    assert_equal ~printer:string_of_int_list
    result expeced

let suite =
    "Util tests" >:::
        [
            "test_rm_el_middle" >:: test_rm_el_middle;
            "test_rm_el_first" >:: test_rm_el_first;
            "test_rm_el_last" >:: test_rm_el_last;
        ]

let _ = run_test_tt_main suite

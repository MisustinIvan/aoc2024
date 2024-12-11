(** Updates a given stone according to the rules described in the puzzle. *)
let update num =
    let str_num = string_of_int num in
    let len = String.length str_num in
    match num with
    | 0 -> [1]
    | _ when (len mod 2) = 0 ->
        let mid = len / 2 in
        [
          int_of_string (String.sub str_num 0 mid);
          int_of_string (String.sub str_num mid mid)
        ]
    | _ -> [num * 2024]

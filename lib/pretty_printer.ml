(** Returns the string of a int list formatted as such [1; 2; 3]. *)
let string_of_list_int lst =
    Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_int lst))

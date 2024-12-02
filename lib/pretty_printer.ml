(** Returns the string of a int list formatted as such [1; 2; 3]. *)
let string_of_int_list lst =
    Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_int lst))

    (** Returns the string of a string list formatted as such [ligma; balls]. *)
let string_of_string_list lst =
    Printf.sprintf "[%s]" (String.concat "; "  lst)

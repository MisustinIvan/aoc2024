let repeat_a a n =
    let rec aux acc n =
        match n with
        | 0 -> acc
        | _ -> aux (a :: acc) (n-1)
    in aux [] n

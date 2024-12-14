(** Solves a system of 2 linear equations as described in the puzzle. *)
let solve_eq ((ax, ay), (bx, by), (rx, ry)) =
    let divisor = (ax * by) - (ay * bx) in
    if divisor = 0 then
        None
    else if ((-bx * ry + by * rx) mod divisor) != 0 || ((ax * ry - ay * rx) mod divisor) != 0
    then
        None
    else
        let a = (-bx * ry + by * rx) / divisor in
        let b = (ax * ry - ay * rx) / divisor in
        Some (a, b)

(** I really have to learn regex it seems, because this sucks. *)
let parse_input input =
    input
    |> String.concat "\n"
    |> Str.split (Str.regexp "\n\n")
    |> List.map (String.split_on_char '\n')
    |> List.map (function
        | [aln; bln; rln] ->
                let saln = Array.of_list (String.split_on_char ' ' aln) in
                let sbln = Array.of_list (String.split_on_char ' ' bln) in
                let srln = Array.of_list (String.split_on_char ' ' rln) in
                let ax = int_of_string (List.hd(String.split_on_char ',' (List.hd (List.rev (String.split_on_char '+' saln.(2)))))) in
                let ay = int_of_string (List.hd (List.rev (String.split_on_char '+' saln.(3)))) in
                let bx = int_of_string (List.hd(String.split_on_char ',' (List.hd (List.rev (String.split_on_char '+' sbln.(2)))))) in
                let by = int_of_string (List.hd (List.rev (String.split_on_char '+' sbln.(3)))) in
                let rx = int_of_string (List.hd(String.split_on_char ',' (List.hd (List.rev(String.split_on_char '=' srln.(1)))))) in
                let ry = int_of_string (List.hd(List.rev (String.split_on_char '=' srln.(2)))) in
                ((ax, ay), (bx, by), (rx, ry))
        | _ -> failwith "expected three lines"
    )

open Day13_shared

(**
    Solves the puzzle for day 13 part 2, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    (* Process into workable format. *)
    |> parse_input
    |> List.map (fun (pa, pb, (rx, ry)) -> (pa, pb, (rx+10000000000000, ry+10000000000000)))
    |> List.map solve_eq
    |> List.filter_map Fun.id
    |> List.map (fun (ap, bp) -> (3*ap)+bp)
    |> List.fold_left (+) 0

open Day08_shared

(**
    Solves the puzzle for day 7 part 1, loading input from given filename.
*)
let solve filename = filename
    |> Input_reader.read_input
    |> List.map (String.to_seq)
    |> List.map (Array.of_seq)
    |> Array.of_list
    (* Find all the antennas. *)
    |> fun table -> (Array.fold_left (fun acc (ridx, ln) ->
        Array.fold_left (fun acc (cidx, el) ->
            if el != '.' then (el, ridx, cidx) :: acc else acc
        ) acc (Array.mapi (fun cidx el -> (cidx, el)) ln)
    ) [] (Array.mapi (fun ridx ln -> (ridx, ln)) table), table)
    |> fun (ats, table) -> (cartesian ats, table)
    |> fun (cts, table) -> List.iter (fun ((f1, r1, c1), (f2, r2, c2)) ->
            if r1 = r2 && c1 = c2 then ()
            else if f1 = f2 then
                let dr = r1 - r2 in
                let dc = c1 - c2 in
                (* Also create an antinode the the postion of the antennas. *)
                ignore(set_el table (r1) (c1) '#');
                ignore(set_el table (r2) (c2) '#');
                let rec aux (p1r, p1c) (p2r, p2c) =
                if (set_el table (p1r+dr) (p1c+dc) '#')
                || (set_el table (p2r-dr) (p2c-dc) '#')
                then aux ((p1r+dr), (p1c+dc)) ((p2r-dr), (p2c-dc))
                else ()
                in aux (r1, c1) (r2, c2)
            else ()
    ) cts; table
    |> Array.fold_left (fun acc ln ->
            Array.fold_left (fun acc el -> if el = '#' then acc+1 else acc) acc ln
    ) 0

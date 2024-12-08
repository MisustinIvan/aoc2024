(** Retrieves the element at given row and column in the table, if oob, returns '.' as described in puzzle. *)
let get_el table row colum =
    if row < 0 || row >= Array.length table
    then '.'
    else if colum < 0 || colum >= Array.length (Array.get table row)
    then '.'
    else Array.get (Array.get table row) colum

(** Sets the element in the table at position to the argument if the spot is empty as described in puzzle. *)
let set_el table row column el =
    if row < 0 || row >= Array.length table
    then ()
    else if column < 0 || column >= Array.length (Array.get table row)
    then ()
    else table.(row).(column) <- el


(** Generates a cartesian product of a single list. *)
let cartesian list =
    List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) list) list)


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
                set_el table (r1+dr) (c1+dc) '#';
                set_el table (r2-dr) (c2-dc) '#';
            else ()
    ) cts; table
    |> Array.fold_left (fun acc ln ->
            Array.fold_left (fun acc el -> if el = '#' then acc+1 else acc) acc ln
    ) 0

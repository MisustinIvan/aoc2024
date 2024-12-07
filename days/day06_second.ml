open Day06_shared

(** Checks for infinite loops. *)
let check_loop table (posx, posy) bonus_pos =
            let rec aux table (posx, posy) (dx, dy) acc =
                let next_pos = (posx + dx, posy + dy) in
                let next_el = if next_pos = bonus_pos then '#' else get_el table next_pos in
                (*Printf.printf "%s\n" (table_string table);*)
                if List.exists (fun el -> el = ((posx, posy), (dx,dy))) acc then true
                else
                match next_el with
                | '0' -> false
                | '.' | 'x' -> aux table next_pos (dx, dy) (((posx,posy), (dx, dy)):: acc)
                | '#' -> aux table (posx, posy) (turn_right (dx,dy)) (((posx,posy), (dx, dy)):: acc)
                | _ as tk -> failwith ("unexpected token: " ^ (String.make 1 tk))
            in aux table (posx, posy) (0,-1) []


let find_visited table = table
    |> Array.mapi (fun ridx row ->
        Array.mapi (fun cidx el ->
            if el = 'x' then (cidx, ridx)
            else (-1, -1)
        ) row
    )
    |> Array.fold_left (fun acc row ->
        (Array.fold_left (fun acc el ->
            match el with
            | (-1, -1) -> acc
            | (cidx, ridx) -> (cidx, ridx) :: acc
        ) acc row)
    ) []

(**
    Solves the puzzle for day 6 part 2, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    (* Convert to usable format. *)
    |> List.map (fun ln -> ln |> String.to_seq |> Array.of_seq)
    (* Extract the position of the guard. *)
    |> fun table -> (Array.of_list table, find_guard table)
    (* Walk through the table, marking all the visited fields. *)
    |> fun (table, guard_pos) -> (walk_table (table, guard_pos), table, guard_pos)
    |> fun (walked_table, unwalked_table, guard_pos) ->
            (walked_table, unwalked_table, guard_pos)
    |> fun (walked_table, unwalked_table, guard_pos) -> (find_visited walked_table, unwalked_table, guard_pos)
    |> fun (visited, table, guard_pos) ->
            (visited, table, guard_pos)
    |> fun (visited, table, guard_pos) ->
            List.map (fun pos ->
                if (check_loop table guard_pos pos)
                then 1
                else 0
            ) visited
    |> List.fold_left (+) 0

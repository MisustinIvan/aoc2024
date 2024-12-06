(** Retrieves the element at given row and column in the table, if oob, returns '.' as described in puzzle. *)
let get_el table (column, row) =
    if row < 0 || row >= Array.length table
    then '0'
    else if column < 0 || column >= Array.length (Array.get table row)
    then '0'
    else Array.get (Array.get table row) column

let set_el table (column, row) value =
    if row < 0 || row >= Array.length table
    then table
    else if column < 0 || column >= Array.length (Array.get table row)
    then table
    else
        (table.(row).(column) <- value; table)

(** Takes in the vector (dx, dy) and rotates it right.
    It must be noted that the coordinates grow to the right
    because of the way the way the array indices are.*)
let turn_right (dx, dy) =
    match (dx, dy) with
    | (1,0) -> (0,1)
    | (0,1) -> (-1, 0)
    | (-1,0) -> (0, -1)
    | (0,-1) -> (1,0)
    | _ -> failwith "Invalid direction input."

(** Returns the position of the guard in the table. *)
let find_guard table =
    let rec aux tbl ridx =
        match tbl with
        | current :: rest -> (
            let cidx = Array.find_index(fun el -> el == '^') current in
            match cidx with
            | Some(cidx) -> (cidx, ridx)
            | None -> aux rest (ridx+1)
        )
        | [] -> failwith "Guard not found."
    in aux table 0

(** Returns the string of a char list list. *)
let table_string table = table
    |> Array.map (fun row -> row
            |> Array.to_list
            |> List.map (fun el -> String.make 1 el)
            |> String.concat "")
    |> Array.to_list
    |> String.concat "\n"

(**
    Solves the puzzle for day 6 part 1, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    (* Convert to usable format. *)
    |> List.map (fun ln -> ln |> String.to_seq |> Array.of_seq)
    (* Extract the position of the guard. *)
    |> fun table -> (Array.of_list table, find_guard table)
    (* Walk through the table until hitting the edge. *)
    |> fun (table, (posx, posy)) ->
            let rec aux table (posx, posy) (dx, dy) =
                let next_pos = (posx + dx, posy + dy) in
                let next_el = get_el table next_pos in
                match next_el with
                | '0' -> set_el table (posx, posy) 'x'
                | '.' | 'x' -> aux (set_el (set_el table (posx, posy) 'x') next_pos '^') next_pos (dx, dy)
                | '#' -> aux table (posx, posy) (turn_right (dx,dy))
                | _ as tk -> failwith ("unexpected token: " ^ (String.make 1 tk))
            in aux table (posx, posy) (0,-1)
    (* Count visited fields *)
    |> fun table -> Array.fold_left (fun acc row ->
            acc + (Array.fold_left (fun acc el -> if el == 'x' then acc+1 else acc) 0 row)
    ) 0 table

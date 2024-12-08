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
    then false
    else if column < 0 || column >= Array.length (Array.get table row)
    then false
    else (table.(row).(column) <- el; true)

(** Generates a cartesian product of a single list. *)
let cartesian list =
    List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) list) list)

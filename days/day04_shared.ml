(** Retrieves the element at given row and column in the table, if oob, returns '.' as described in puzzle. *)
let get_el table row colum =
    if row < 0 || row >= Array.length table
    then '.'
    else if colum < 0 || colum >= Array.length (Array.get table row)
    then '.'
    else Array.get (Array.get table row) colum

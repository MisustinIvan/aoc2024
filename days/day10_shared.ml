(** Retrieves the element at given row and column in the table, if oob, returns '.' as described in puzzle. *)
let get_el table column row =
    if row < 0 || row >= Array.length table
    then -1
    else if column < 0 || column >= Array.length (Array.get table row)
    then -1
    else table.(row).(column)

(** Finds all the elemens with value 0 and their position in the given table. *)
let find_min (table : int list list) =
    let els = List.flatten ( List.mapi (fun ridx ln -> List.mapi (fun cidx el -> (el, (cidx, ridx))) ln) table) in
    List.filter (fun (el, _) -> el = 0) els

(** Returns the neighbors of the given position in the cardinal directions. *)
let get_neighbors table ((cidx : int), (ridx : int)) =
    [
        (get_el table (cidx) (ridx+1), ((cidx), (ridx+1)));
        (get_el table (cidx) (ridx-1), ((cidx), (ridx-1)));
        (get_el table (cidx+1) (ridx), ((cidx+1), (ridx)));
        (get_el table (cidx-1) (ridx), ((cidx-1), (ridx)))
    ]


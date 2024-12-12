module IntPairs =
    struct
        type t = int * int
        let compare (x0,y0) (x1,y1) =
        match Stdlib.compare x0 x1 with
            0 -> Stdlib.compare y0 y1
            | c -> c
    end

module PairsMap = Map.Make(IntPairs)

let get_el table column row =
    if row < 0 || row >= Array.length table
    then '.'
    else if column < 0 || column >= Array.length (Array.get table row)
    then '.'
    else table.(row).(column)

let get_neighbors_card table ((cidx : int), (ridx : int)) =
    [
        (get_el table (cidx) (ridx+1), ((cidx), (ridx+1)));
        (get_el table (cidx) (ridx-1), ((cidx), (ridx-1)));
        (get_el table (cidx+1) (ridx), ((cidx+1), (ridx)));
        (get_el table (cidx-1) (ridx), ((cidx-1), (ridx)));
    ]

let get_neighbors_diag table ((cidx : int), (ridx : int)) =
    [
        (get_el table (cidx+1) (ridx+1), ((cidx+1), (ridx+1)));
        (get_el table (cidx+1) (ridx-1), ((cidx+1), (ridx-1)));
        (get_el table (cidx-1) (ridx-1), ((cidx-1), (ridx-1)));
        (get_el table (cidx-1) (ridx+1), ((cidx-1), (ridx+1)));
    ]

let get_n_corners card_els diag_els =
    let gc (el, _) = el in
    let c = Array.of_list card_els in
    let d = Array.of_list diag_els in
    List.fold_left (+) 0 [
        if gc c.(0) = gc c.(2) && gc c.(0) = '.' then 1 else if gc c.(0) = gc c.(2) && gc d.(0) = '.' then 1 else 0;
        if gc c.(0) = gc c.(3) && gc c.(0) = '.' then 1 else if gc c.(0) = gc c.(3) && gc d.(3) = '.' then 1 else 0;
        if gc c.(1) = gc c.(2) && gc c.(1) = '.' then 1 else if gc c.(1) = gc c.(2) && gc d.(1) = '.' then 1 else 0;
        if gc c.(1) = gc c.(3) && gc c.(1) = '.' then 1 else if gc c.(1) = gc c.(3) && gc d.(2) = '.' then 1 else 0;
    ]


(**
    Solves the puzzle for day 12 part 1, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    |> List.map (fun ln -> Array.of_seq(String.to_seq ln))
    |> Array.of_list
    |> fun table ->
            let visited = ref PairsMap.empty in
            let rec map (cidx, ridx) =
                match PairsMap.find_opt (cidx, ridx) !visited with
                | Some _ -> (0,0)
                | None ->
                    visited := PairsMap.add (cidx, ridx) true !visited;
                    let cel = get_el table cidx ridx in
                    let car_nbs = List.map (fun (el, pos) -> if el = cel then (el, pos) else ('.', pos)) (get_neighbors_card table (cidx, ridx)) in
                    let diag_nbs = List.map (fun (el, pos) -> if el = cel then (el, pos) else ('.', pos)) (get_neighbors_diag table (cidx, ridx)) in

                    let (corners : int) = get_n_corners car_nbs diag_nbs in

                    let sm_nbs = List.filter (fun (el, _) -> el = cel) car_nbs in
                    let sm_nbs_vls = List.map (fun (_, pos) -> map pos) sm_nbs in
                    List.fold_left (fun (acels, accrs) (els, crs) -> (acels + els, accrs + crs)) (1, corners) sm_nbs_vls
            in
            let mpd = Array.mapi (
                fun ridx ln ->
                    Array.mapi (
                        fun cidx el -> (map (cidx, ridx), el)
                    ) ln
            ) table
            in
            Array.fold_left (
                fun acc ln ->
                    Array.fold_left (
                        fun acc ((els, crs), _) -> acc + (els*crs)
                    ) acc ln
            ) 0 mpd

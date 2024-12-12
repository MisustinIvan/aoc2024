module IntPairs =
    struct
        type t = int * int
        let compare (x0,y0) (x1,y1) =
        match Stdlib.compare x0 x1 with
            0 -> Stdlib.compare y0 y1
            | c -> c
    end

module PairsMap = Map.Make(IntPairs)

let get_el table row column =
    if row < 0 || row >= Array.length table
    then '.'
    else if column < 0 || column >= Array.length (Array.get table row)
    then '.'
    else table.(row).(column)

let get_neighbors table ((cidx : int), (ridx : int)) =
    [
        (get_el table (cidx) (ridx+1), ((cidx), (ridx+1)));
        (get_el table (cidx) (ridx-1), ((cidx), (ridx-1)));
        (get_el table (cidx+1) (ridx), ((cidx+1), (ridx)));
        (get_el table (cidx-1) (ridx), ((cidx-1), (ridx)))
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
            let rec map (ridx, cidx) =
                match PairsMap.find_opt (ridx, cidx) !visited with
                | Some _ -> (0,0)
                | None ->
                    visited := PairsMap.add (ridx, cidx) true !visited;
                    let cel = get_el table ridx cidx in
                    let nbs = get_neighbors table (ridx, cidx) in
                    let edges = List.filter (fun (el, _) -> el != cel) nbs in
                    let snbs = List.filter (fun (el, _) -> el = cel) nbs in
                    let snbvs = List.map (fun (_, pos) -> map pos) snbs in
                    List.fold_left (fun (acels, acegs) (els, egs) -> (acels + els, acegs + egs)) (1,List.length edges) snbvs
            in
            let mpd = Array.mapi (
                fun ridx ln ->
                    Array.mapi (
                        fun cidx el -> (map (ridx, cidx), el)
                    ) ln
            ) table
            in
            Array.fold_left (
                fun acc ln ->
                    Array.fold_left (
                        fun acc ((posx, posy), _) -> acc + (posx*posy)
                    ) acc ln
            ) 0 mpd

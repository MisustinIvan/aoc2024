open Day20_shared

let cheat_moves = [ {x = 0; y = -2}; {x = 0; y = 2}; {x = -2; y = 0}; {x = 2; y = 0}; {x = 1; y = 1}; {x = -1; y = -1}; {x = 1; y = -1}; {x = -1; y = 1} ]

let solve_maze_all (maze : block array array) =
    let start_pos = find_block maze Start in
    let end_pos = find_block maze End in
    let best_solution = dijkstra maze start_pos end_pos in
    let path = reconstruct_path best_solution in
    let visited = Hashtbl.create (List.length path) in
    List.iteri (fun i pos -> Hashtbl.replace visited pos i) path;
    let diffs = List.mapi (fun i pos -> 
        List.map ( fun v ->
            let np = pos_add pos v in
            if not (Hashtbl.mem visited np) then 0 else
            (Hashtbl.find visited np) - i - 2
        ) cheat_moves
    ) path in

    (*Printf.printf "DIFFS: %s\n" (Pretty_printer.string_of_int_list (List.flatten diffs));*)

    List.length (List.filter (fun diff -> diff >= 100) (List.flatten diffs))

let solve filename =
    filename
    |> Input_reader.read_input
    |> parse_input
    |> solve_maze_all

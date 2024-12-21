open Day20_shared

let solve_maze_all maze =
    let start_pos = find_block maze Start in
    let end_pos = find_block maze End in
    let best_solution = dijkstra maze start_pos end_pos in
    let path = reconstruct_path best_solution in
    let visited = Hashtbl.create (List.length path) in
    let offsets = 
        List.init 41 (fun row ->
            List.init 41 (fun col ->
                { x = col - 20; y = row - 20 }
            )
        ) |> List.flatten |> List.filter (fun offset -> (manhattan_mag offset) <= 20) |> List.filter (fun offset -> (manhattan_mag offset) >= 1) in
    List.iteri (fun i pos -> Hashtbl.replace visited pos i) path;
    let diffs = List.mapi (fun i pos ->
        List.filter_map (fun offset ->
            let np = pos_add pos offset in
            if not (Hashtbl.mem visited np) then None else
            let save = (Hashtbl.find visited np) - i - (abs offset.x) - (abs offset.y) in
            if save >= 100 then Some save else None
        ) offsets
    ) path in

    List.length (List.flatten diffs)

let solve filename =
    filename
    |> Input_reader.read_input
    |> parse_input
    |> solve_maze_all

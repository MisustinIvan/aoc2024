open Day16_shared

let print_enabled = false

(** Draws the path in the given maze. *)
let visualize_path (maze : block array array) (path : pos list) =
    let visual_maze = Array.map (fun ln -> Array.copy ln |> Array.map string_of_block) maze in
    List.iter (fun { x; y } ->
        match maze.(y).(x) with
        | Space -> visual_maze.(y).(x) <- "\x1b[31m*\x1b[0m"
        | Start | End -> ()
        | _ -> ()
    ) path;
    visual_maze
    |> Array.map (fun ln -> String.concat "" (ln |> Array.to_list))
    |> Array.to_list
    |> String.concat "\n"

let dijkstra (maze : block array array) (start_pos : pos) (end_pos : pos) =
    let rows = Array.length maze in
    let cols = Array.length maze.(0) in
    (* stores visited positions and directions *)
    let visited = Hashtbl.create 1000 in
    let rec search queue =
        (* if queue is empty, that means we found nothing *)
        if StateQueue.is_empty queue then
            failwith "no solution found"
        else
            let (current, queue) = StateQueue.pop queue in
            (* if reached end, return the path *)
            if current.pos = end_pos then
                (if print_enabled then Printf.printf "%s\n" (visualize_path maze (reconstruct_path current));
                current.cost)
            (* check if it makes sense to explore this path *)
            else if Hashtbl.mem visited (current.pos.x, current.pos.y, current.dir) then
                search queue
            else (
                (* get neighbors and update their costs based on the rotations and movement needed to get to their locaiton *)
                Hashtbl.add visited (current.pos.x, current.pos.y, current.dir) true;
                let neighbors =
                    List.filter_map (fun (dx, dy, new_dir) -> 
                        let nx, ny = current.pos.x + dx, current.pos.y + dy in
                        if nx >= 0 && ny >= 0 && nx < cols && ny < rows && maze.(ny).(nx) <> Wall then
                            let move_cost = current.cost + 1 + rotate_cost current.dir new_dir in
                            Some {pos = {x = nx; y = ny}; dir = new_dir; cost = move_cost; parent = Some current}
                        else None
                    ) moves
                in
                (* insert them into the queue *)
                let queue = List.fold_left (fun (q : StateQueue.t) (n : state) ->
                    StateQueue.insert q { n with cost = n.cost }
                ) queue neighbors in
                search queue
            )
    in
    let initial_state = { pos = start_pos; dir = Right; cost = 0; parent = None} in
    search (StateQueue.insert StateQueue.empty initial_state)

let solve_maze (maze : block array array) =
    let start_pos = find_block maze Start in
    let end_pos = find_block maze End in
    dijkstra maze start_pos end_pos

let solve filename =
    filename
    |> Input_reader.read_input
    |> parse_input
    |> solve_maze

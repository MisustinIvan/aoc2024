open Day16_shared

let print_enabled = false

(** Draws the paths in the given maze. *)
let visualize_paths (maze : block array array) (paths : pos list list) =
    let visual_maze = Array.map (fun ln -> Array.copy ln |> Array.map string_of_block) maze in
    List.iter (fun sol -> List.iter (fun { x; y } ->
        match maze.(y).(x) with
        | Space -> visual_maze.(y).(x) <- "\x1b[31m*\x1b[0m"
        | Start | End -> ()
        | _ -> ()
    ) sol) paths;
    visual_maze
    |> Array.map (fun ln -> String.concat "" (ln |> Array.to_list))
    |> Array.to_list
    |> String.concat "\n"

(** Counts all visited tiles by the paths through the maze... (this function sucks ik) *)
let count_tiles (maze : block array array) (paths : pos list list) =
    let mc = Array.map (fun ln -> Array.copy ln |> Array.map string_of_block) maze in
    List.iter (fun sol -> List.iter (fun { x; y } ->
        mc.(y).(x) <- "*"
    ) sol) paths;

    mc
    |> Array.map (fun ln -> String.concat "" (ln |> Array.to_list))
    |> Array.to_list
    |> String.concat "\n"
    |> String.to_seq
    |> List.of_seq
    |> List.fold_left (fun acc el -> if el = '*' then acc+1 else acc) 0

let a_star_all (maze : block array array) (start_pos : pos) (end_pos : pos) =
    let rows = Array.length maze in
    let cols = Array.length maze.(0) in
    (* stores the best cost for a given coordinate and direction that was already visited *)
    let best_cost = Hashtbl.create 1000 in
    (* keeps track of the solutions *)
    let solutions = ref [] in

    let rec search queue =
        (* if we don't have any states to explore, return solutions *)
        if StateQueue.is_empty queue then
            if !solutions = [] then
                failwith "no solution found"
            else
                !solutions
        else
            (* process next state *)
            let (current, queue) = StateQueue.pop queue in
            if current.pos = end_pos then (
                (* if reached end and cost is the same as the best cost, add to solutions*)
                if !solutions = [] || current.cost = (List.hd !solutions).cost then
                    solutions := current :: !solutions;
                search queue
            )
            else (
                (* key for frequent hashtbl access... *)
                let key = (current.pos.x, current.pos.y, current.dir) in
                (* check if it makes sense to continue exploring this path *)
                if Hashtbl.mem best_cost key && Hashtbl.find best_cost key < current.cost then
                    search queue
                else (
                    Hashtbl.replace best_cost key current.cost;
                    (* get neighbors and update the cost based on the move and rotations required to get to those positions *)
                    let neighbors =
                        List.filter_map (fun (dx, dy, new_dir) ->
                            let nx, ny = current.pos.x + dx, current.pos.y + dy in
                            if nx >= 0 && ny >= 0 && nx < cols && ny < rows && maze.(ny).(nx) <> Wall then
                                let move_cost = current.cost + 1 + rotate_cost current.dir new_dir in
                                Some {pos = {x = nx; y = ny}; dir = new_dir; cost = move_cost; parent = Some current}
                            else None
                        ) moves
                    in
                    (* insert all of them into the queue *)
                    let queue = List.fold_left (fun q n -> StateQueue.insert q n) queue neighbors in
                    search queue
                )
            )
    in
    let initial_state = { pos = start_pos; dir = Right; cost = 0; parent = None } in
    search (StateQueue.insert StateQueue.empty initial_state)

let solve_maze_all (maze : block array array) =
    let start_pos = find_block maze Start in
    let end_pos = find_block maze End in
    (* finds all the possible paths through the graph of the same lowest cost *)
    let solutions = a_star_all maze start_pos end_pos in
    if print_enabled then Printf.printf "found %d paths with cost %d\n" (List.length solutions) (List.hd solutions).cost;
    let paths = List.map reconstruct_path solutions in
    if print_enabled then Printf.printf "Path:\n%s\n\n" (visualize_paths maze paths);
    count_tiles maze paths

let solve filename =
    filename
    |> Input_reader.read_input
    |> parse_input
    |> solve_maze_all

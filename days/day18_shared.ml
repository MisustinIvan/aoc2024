type block =
    | Wall
    | Space

let string_of_block (block : block) =
    match block with
    | Space -> "."
    | Wall -> "#"

type pos = {
    x: int;
    y: int;
}

type state = {
    pos: pos;
    cost: int;
    parent: state option;
}

let rec reconstruct_path state =
    match state.parent with
    | None -> [state.pos]
    | Some parent -> reconstruct_path parent @ [state.pos]

module StateQueue = struct
    type t = state list
    let empty = []
    let is_empty q = (q = [])
    let insert q state = List.sort (fun s1 s2 -> compare s1.cost s2.cost) (state :: q)
    let pop = function
        | [] -> failwith "queue is empty"
        | hd :: tl -> (hd, tl)
    end

let moves = [ {x = 0; y = -1}; {x = 0; y = 1}; {x = -1; y = 0}; {x = 1; y = 0} ]

let dijkstra (maze : block array array) (start_pos : pos) (end_pos : pos) =
    let rows = Array.length maze in
    let cols = Array.length maze.(0) in
    (* stores visited positions and directions *)
    let visited = Hashtbl.create 1000 in
    let rec search queue =
        (* if queue is empty, that means we found nothing *)
        if StateQueue.is_empty queue then
            None
        else
            let (current, queue) = StateQueue.pop queue in
            (* if reached end, return the path *)
            if current.pos = end_pos then Some current
            (* check if it makes sense to explore this path *)
            else if Hashtbl.mem visited current.pos
            then search queue
            else (
                (* get neighbors and update their costs based on the rotations and movement needed to get to their locaiton *)
                Hashtbl.add visited current.pos true;
                let neighbors =
                    List.filter_map (fun move -> 
                        let nx, ny = current.pos.x + move.x, current.pos.y + move.y in
                        if nx >= 0 && ny >= 0 && nx < cols && ny < rows && maze.(ny).(nx) <> Wall then
                            Some {pos = {x = nx; y = ny}; cost = current.cost + 1; parent = Some current}
                        else None
                    ) moves
                in
                (* insert them into the queue *)
                let queue = List.fold_left (fun (q : StateQueue.t) (n : state) ->
                    StateQueue.insert q n
                ) queue neighbors in
                search queue
            )
    in
    let initial_state = { pos = start_pos; cost = 0; parent = None} in
    search (StateQueue.insert StateQueue.empty initial_state)

(** Draws the path in the given maze. *)
let visualize_path (maze : block array array) (path : pos list) =
    let visual_maze = Array.map (fun ln -> Array.copy ln |> Array.map string_of_block) maze in
    List.iter (fun { x; y } ->
        match maze.(y).(x) with
        | Space -> visual_maze.(y).(x) <- "\x1b[31m*\x1b[0m"
        | _ -> ()
    ) path;
    visual_maze
    |> Array.map (fun ln -> String.concat "" (ln |> Array.to_list))
    |> Array.to_list
    |> String.concat "\n"

type block =
    | Wall
    | Space
    | Start
    | End

let parse_block (block : char) =
    match block with
    | '#' -> Wall
    | '.' -> Space
    | 'S' -> Start
    | 'E' -> End
    | ic -> failwith (Printf.sprintf "invalid character: %c" ic)

let string_of_block (block : block) =
    match block with
    | Start -> "S"
    | End -> "E"
    | Space -> "."
    | Wall -> "#"

type pos = {
    x: int;
    y: int;
}

let pos_add p1 p2 =
    {x = p1.x + p2.x; y = p1.y + p2.y}

let manhattan p1 p2 =
    abs (p1.x - p2.x) + abs (p1.y - p2.y)

let manhattan_mag p =
    (abs p.x) + (abs p.y)

type state = {
    pos: pos;
    time: int;
    parent: state option;
}

let find_block (maze : block array array) (b : block) =
    let rec aux mz ridx =
        match mz with
        | current :: rest -> (
            let cidx = Array.find_index(fun el -> el = b) current in
            match cidx with
            | Some(cidx) -> { x = cidx; y = ridx}
            | None -> aux rest (ridx+1)
        )
        | [] -> failwith "Block not found."
    in aux (Array.to_list maze) 0

module StateQueue = struct
    type t = state list
    let empty = []
    let is_empty q = (q = [])
    let insert q state = List.sort (fun s1 s2 -> compare s2.time s1.time) (state :: q)
    let pop = function
        | [] -> failwith "queue is empty"
        | hd :: tl -> (hd, tl)
    end

let moves = [ {x = 0; y = -1}; {x = 0; y = 1}; {x = -1; y = 0}; {x = 1; y = 0} ]

let rec reconstruct_path state =
    match state.parent with
    | None -> [state.pos]
    | Some parent -> reconstruct_path parent @ [state.pos]


let parse_input (input : string list) = input
    |> Array.of_list
    |> Array.map (fun ln -> ln |> String.to_seq |> Array.of_seq |> Array.map parse_block )

let dijkstra (maze : block array array) (start_pos : pos) (end_pos : pos) =
    let rows = Array.length maze in
    let cols = Array.length maze.(0) in

    let visited = Hashtbl.create 1000 in

    let rec search queue =
        (* if we don't have any states to explore, return solutions *)
        if StateQueue.is_empty queue then failwith "no solution found"
        else
            (* process next state *)
            let (current, queue) = StateQueue.pop queue in
            if current.pos = end_pos then ( current )
            else (
                (* key for frequent hashtbl access... *)
                let key = (current.pos.x, current.pos.y) in
                (* check if it makes sense to continue exploring this path *)
                if Hashtbl.mem visited key then search queue
                else (
                    Hashtbl.replace visited key current.time;
                    (* get neighbors and update the cost *)
                    let neighbors =
                        List.filter_map (fun v ->
                            let nx, ny = current.pos.x + v.x, current.pos.y + v.y in
                            if nx >= 0 && ny >= 0 && nx < cols && ny < rows && maze.(ny).(nx) <> Wall then
                                Some {pos = {x = nx; y = ny}; time = current.time + 1; parent = Some current}
                            else None
                        ) moves
                    in
                    (* insert all of them into the queue *)
                    let queue = List.fold_left (fun q n -> StateQueue.insert q n) queue neighbors in
                    search queue
                )
            )
    in
    let initial_state = { pos = start_pos; time = 0; parent = None } in
    search (StateQueue.insert StateQueue.empty initial_state)

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

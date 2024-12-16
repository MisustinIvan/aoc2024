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

type direction =
    | Up
    | Down
    | Left
    | Right

type pos = {
    x: int;
    y: int;
}

type state = {
    pos: pos;
    dir: direction;
    cost: int;
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
    let insert q state = List.sort (fun s1 s2 -> compare s1.cost s2.cost) (state :: q)
    let pop = function
        | [] -> failwith "queue is empty"
        | hd :: tl -> (hd, tl)
    end

let moves = [ (0, -1, Up); (0, 1, Down); (-1, 0, Left); (1, 0, Right) ]

let rotate_cost dir1 dir2 =
    match (dir1, dir2) with
    | (Up, Down) | (Down, Up) | (Left, Right) | (Right, Left) -> 2000
    | (Up, Left) | (Left, Down) | (Down, Right) | (Right, Up)
    | (Up, Right) | (Right, Down) | (Down, Left) | (Left, Up) -> 1000
    | _ -> 0

let rec reconstruct_path state =
    match state.parent with
    | None -> [state.pos]
    | Some parent -> reconstruct_path parent @ [state.pos]


let parse_input (input : string list) = input
    |> Array.of_list
    |> Array.map (fun ln -> ln |> String.to_seq |> Array.of_seq |> Array.map parse_block )

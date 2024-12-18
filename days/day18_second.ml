open Day18_shared

let printing_enabled = false

let solve filename =
    filename
    |> Input_reader.read_input
    |> List.map (fun ln -> String.split_on_char ',' ln |> List.map int_of_string |> Array.of_list)
    |> fun nums ->
            let maze = (Array.init 71 (fun _ -> Array.init 71 (fun _ -> Space))) in
            let nums = let rec populate_maze els i = 
                if i = 1024 then
                    els
                else
                    match els with
                    | head :: rest ->
                            let x = head.(0) in
                            let y = head.(1) in
                            maze.(y).(x) <- Wall;
                            populate_maze rest (i+1)
                    | [] -> []
            in populate_maze nums 0 in

            let rec aux nums (i : int) =
                match nums with
                | head :: rest ->
                    let x = head.(0) in
                    let y = head.(1) in
                    maze.(y).(x) <- Wall;
                    (match (dijkstra maze {x = 0; y = 0} {x = 70; y = 70}) with
                        | Some (_) -> aux rest (i+1)
                        | None -> (string_of_int x) ^ "," ^ (string_of_int y))
                | [] -> "NONE"
            in aux nums 1024

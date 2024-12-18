open Day18_shared

let printing_enabled = false

let solve filename =
    filename
    |> Input_reader.read_input
    |> List.map (fun ln -> String.split_on_char ',' ln |> List.map int_of_string |> Array.of_list)
    |> fun nums ->
            let maze = (Array.init 71 (fun _ -> Array.init 71 (fun _ -> Space))) in
            let rec aux els i = 
                if i = 1024 then
                    ()
                else
                    match els with
                    | head :: rest ->
                            let x = head.(0) in
                            let y = head.(1) in
                            maze.(y).(x) <- Wall;
                            aux rest (i+1)
                    | [] -> ()
            in aux nums 0;
            if printing_enabled then Printf.printf "%s\n" (visualize_path maze []);
            let res = dijkstra maze {x = 0; y = 0} {x = 70; y = 70} in
            match res with
            | Some (res) ->
                let path = reconstruct_path res in
                if printing_enabled then Printf.printf "%s\n" (visualize_path maze path);
                res.cost
            | None -> failwith "could not find path"

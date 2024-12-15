open Day15_shared

let solve filename =
    filename
    |> Input_reader.read_input
    |> process_input
    |> fun (map, cmd) ->
        let rec move (posx, posy) dir =
            let (dx, dy) = (match dir with
            | '>' -> (1,0)
            | '<' -> (-1,0)
            | '^' -> (0,-1)
            | 'v' -> (0,1)
            | _ -> failwith "expected one of 4 directions") in
            let (nposx, nposy) = (posx+dx, posy+dy) in
            let nel = get_el map (nposx, nposy) in
            match nel with
            | '.' -> (
                (map.(nposy).(nposx) <- get_el map (posx, posy);
                map.(posy).(posx) <- '.';
                (nposx, nposy), true)
                )
            | 'O' -> (
                let (_, nelok) = move (nposx, nposy) dir in
                if nelok then
                    (map.(nposy).(nposx) <- get_el map (posx, posy);
                    map.(posy).(posx) <- '.';
                    ((nposx, nposy), true))
                else
                    ((posx, posy), false)
            )
            | '#' -> ((posx, posy), false)
            | _ -> failwith "unexpected element"
        in

        let rec aux bpos cmds =
            match cmds with
            | head :: tail ->(
                let (npos, _) = move bpos head in
                aux npos tail
            )
            | [] -> ()
        in
        aux (find_bot map) cmd;
        find_boxes map 'O'
    |> List.fold_left (fun acc (px, py) -> acc + (px + (py*100))) 0

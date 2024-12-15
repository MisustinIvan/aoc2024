open Day15_shared

let dir_to_cord dir =
    match dir with
    | '>' -> (1,0)
    | '<' -> (-1,0)
    | '^' -> (0,-1)
    | 'v' -> (0,1)
    | _ -> failwith "expected one of 4 directions"

let valid map (px, py) (dx, dy) =
    let rec check (x, y) =
        match get_el map (x, y) with
        | '#' -> false
        | '.' -> true
        | '[' -> if dy = 0 then check (x+dx, y+dy)
                else check (x+dx, y+dy) && check (x+dx+1, y+dy)
        | ']' -> if dy = 0 then check (x+dx, y+dy)
                else check (x+dx, y+dy) && check (x+dx-1, y+dy)
        | ie -> failwith (Printf.sprintf "unexpected character: %c while checking validity"  ie)
    in
    check (px, py)

let swap map (x1, y1) (x2, y2) =
    let e1 = map.(y1).(x1) in
    let e2 = map.(y2).(x2) in
    map.(y1).(x1) <- e2;
    map.(y2).(x2) <- e1;
    ()

let move map (px, py) (dx, dy) =
    let rec move_els (x, y) =
        match get_el map (x, y) with
        (* if free space, swap with element behind *)
        | '.' -> swap map (x, y) (x-dx, y-dy)
        | '[' ->
                if dy = 0 then (
                    move_els (x+dx, y+dy);
                    swap map (x,y) (x-dx, y-dy);
                )
                else (
                    move_els (x+dx, y+dy);
                    move_els (x+dx+1, y+dy);
                    swap map (x, y) (x-dx, y-dy);
                )
        | ']' ->
                if dy = 0 then (
                    move_els (x+dx, y+dy);
                    swap map (x,y) (x-dx, y-dy);
                )
                else (
                    move_els (x+dx, y+dy);
                    move_els (x+dx-1, y+dy);
                    swap map (x, y) (x-dx, y-dy);
                )
        | ie -> failwith (Printf.sprintf "unexpected character: %c while moving els" ie)
    in
    move_els (px, py)


let solve filename =
    filename
    |> Input_reader.read_input
    |> process_input
    (* postprocess input *)
    |> fun (map, cmd) ->
            let res = ref [] in
            Array.iter(fun ln ->
                let ppdln = Array.fold_left (fun acc el -> match el with
                | '@' -> '.' :: '@' :: acc
                | 'O' -> ']' :: '[' :: acc
                | '#' -> '#' :: '#' :: acc
                | '.' -> '.' :: '.' :: acc
                | _ -> failwith "unexpected character"
                ) [] ln in
                res := Array.of_list (List.rev ppdln) :: !res
            ) map;
            (Array.of_list (List.rev !res), cmd)
    |> fun (map, cmd) ->
        let next_iter (posx, posy) dir =
            let (dx, dy) = dir_to_cord dir in
            let next_tile = get_el map (posx+dx, posy+dy) in
            match next_tile with
            | '.' ->
                    swap map (posx,posy) (posx+dx, posy+dy);
                    (posx+dx, posy+dy)
            | '#' -> (posx, posy)
            | '[' | ']' ->
                    if valid map (posx+dx, posy+dy) (dx, dy) then
                        (move map (posx+dx, posy+dy) (dx, dy); (posx+dx, posy+dy))
                    else (posx, posy)
            | ie -> failwith (Printf.sprintf "unexpected character: %c while processing command" ie)
        in
        let rec aux (px, py) cmd =
            match cmd with
            | head :: tail ->
                    let (nx, ny) = next_iter (px, py) head
                    in aux (nx, ny) tail
            | [] -> ()
        in
        aux (find_bot map) cmd;
        List.fold_left (fun acc (posx, posy) -> acc + ((100*posy) + posx)) 0 (find_boxes map '[' )

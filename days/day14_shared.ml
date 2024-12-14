let modulo a b = ((a mod b) + b) mod b

let iter_bot ((px, py), (vx, vy)) (szx, szy) iters =
        match iters with
        | 0 -> (px, py)
        | _ -> (modulo (px + (iters*vx)) (szx), modulo (py + (iters*vy)) (szy))

let print_bots positions (szx, szy) =
    let grid = Array.make_matrix szy szx ".." in

    List.iter (fun (px, py) -> grid.(py).(px) <- "\u{2588}\u{2588}") positions;
    for y = 0 to szy - 1 do
        for x = 0 to szx - 1 do
            print_string grid.(y).(x)
        done;
        print_newline ()
    done; ()

let safety els =
    let mx = 50 in
    let my = 51 in
    List.fold_left (fun (q1, q2, q3, q4) (px, py) -> 
        if px == mx || py == my then (q1, q2, q3, q4) else
        match ((px < mx), (py < my)) with
        (* The actual quadrant does not matter... *)
        | (false, false) -> (q1+1, q2, q3, q4)
        | (false, true) -> (q1, q2+1, q3, q4)
        | (true, false) -> (q1, q2, q3+1, q4)
        | (true, true) -> (q1, q2, q3, q4+1))
    (0,0,0,0) els 
    |> fun (q1, q2, q3, q4) -> q1*q2*q3*q4

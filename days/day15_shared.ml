(** Parses the input into workable format. *)
let process_input input =
    input
    |> String.concat "\n"
    |> Str.split (Str.regexp "\n\n")
    |> (function
        | [map; cmd] -> (map, cmd)
        | _ -> failwith "expected map and commands")
    |> fun (map, cmd) ->
            let pmap =
                map
                |> String.split_on_char '\n'
                |> List.map (fun ln -> ln
                        |> String.to_seq
                        |> Array.of_seq)
                |> Array.of_list
            in
            let pcmd = (Str.global_replace (Str.regexp "\n") "" cmd) |> String.to_seq |> List.of_seq in
            (pmap, pcmd)

let print_map map =
    Array.iter (fun ln -> Array.iter (fun el -> print_char el) ln; print_newline ()) map;
    ()

let print_cmd cmd =
    List.iter (fun el -> print_char el) cmd; print_newline ();
    ()

let get_el table (column, row) =
    if row < 0 || row >= Array.length table
    then '#'
    else if column < 0 || column >= Array.length (Array.get table row)
    then '#'
    else Array.get (Array.get table row) column

let find_bot (table : char array array) =
    let rec aux tbl ridx =
        match tbl with
        | current :: rest -> (
            let cidx = Array.find_index(fun el -> el == '@') current in
            match cidx with
            | Some(cidx) -> (cidx, ridx)
            | None -> aux rest (ridx+1)
        )
        | [] -> failwith "Bot not found."
    in aux (Array.to_list table) 0

let find_boxes arr bc =
    let rows = Array.length arr in
    let cols = Array.length arr.(0) in
    let positions = ref [] in

    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            if arr.(i).(j) = bc then
            positions := (j, i) :: !positions
        done;
    done;
    !positions

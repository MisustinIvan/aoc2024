open Day03_shared

let solve filename =
    Input_reader.read_input filename
    |> List.hd
    |> Str.split (Str.regexp "mul")
    |> List.map (fun expr -> filter expr)
    |> List.fold_left (+) 0

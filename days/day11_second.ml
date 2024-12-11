open Day11_shared
module IntMap = Map.Make(Int)

let memoized f =
    let cache = Hashtbl.create 1000 in
    fun x ->
        match Hashtbl.find_opt cache x with
        | Some n -> n
        | None ->
                let n = f x in
                Hashtbl.add cache x n;
                n

let uniq lst =
    List.fold_left (fun acc x ->
        IntMap.update x (function Some n -> Some (n + 1) | None -> Some 1) acc
    ) IntMap.empty lst

let solve filename =
    filename
    |> Input_reader.read_input
    |> (function
        | [head] -> head
        | _ -> failwith "expected a single line")
    |> String.split_on_char ' '
    |> List.map int_of_string
    |> fun sts ->
        let rec aux sts n =
            match n with
            | 0 -> IntMap.fold (fun _ count acc -> acc + count) sts 0
            | _ ->
                let ns =
                    IntMap.fold (fun nm ct acc ->
                        List.fold_left (fun acc x ->
                            IntMap.update x (function
                                | Some c -> Some (c + ct)
                                | None -> Some ct
                            ) acc
                        ) acc (update nm)
                    ) sts IntMap.empty
                in
                aux ns (n - 1)
        in aux (uniq sts) 75

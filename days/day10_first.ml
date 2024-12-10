open Day10_shared

(** Returns the unique elements of a list. *)
let unique lst =
    let rec aux lst acc =
        match lst with
        | [] -> acc
        | h :: t ->
            if List.mem h acc then aux t acc else aux t (h :: acc)
    in
    aux lst []

(**
    Solves the puzzle for day 10 part 1, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    (* Process into workable format. *)
    |> List.map (fun ln -> ln |> String.to_seq |> Seq.map (fun n -> String.make 1 n |> int_of_string) |> List.of_seq)
    |> fun table -> (
        List.map (fun ln -> Array.of_list ln) table |> Array.of_list,
        find_min table)
    |> fun (table, min) ->
            let
                rec fnd (posx, posy) cn acc = 
                    if cn = 9 then
                        [acc]
                    else
                        let neighbors = get_neighbors table (posx, posy) in
                        let vld = List.filter (fun (el, _) -> el = cn+1) neighbors in
                        List.flatten (List.map (fun (h, p) -> fnd p h ((h, p) :: acc)) vld)
            in
            List.flatten (List.map (fun (n, p) -> fnd p n [(n, p)]) min)
    |> List.map (fun path -> (List.hd path, List.rev path |> List.hd))
    |> unique
    |> List.length

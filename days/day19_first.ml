let rec is_possible target prefixes =
    let rec aux = function
        | [] -> false
        | prefix :: rest ->
            if String.starts_with ~prefix target then
                let remaining = String.sub target (String.length prefix) (String.length target - String.length prefix) in
                is_possible remaining prefixes || aux rest
            else
                aux rest
    in
    target = "" || aux prefixes

let reduce_set cmbs =
    let collect_rest els i =
        let rec aux before after j =
            match after with
            | head :: tail -> if j = i then (List.rev before, tail) else aux (head :: before) (tail) (j+1)
            | [] -> (List.rev before, after)
        in aux [] els 0
    in

    let check el i =
        let (before, after) = collect_rest (cmbs) (i) in
        if is_possible el (before @ after) then None
        else Some el

    in List.filter_map (fun (i, el) -> check el i) (List.mapi (fun i el -> (i, el)) cmbs)


let solve filename =
    filename
    |> Input_reader.read_input
    |> String.concat "\n"
    |> Str.split (Str.regexp "\n\n")
    |> (function
        | [els; cmb] -> (els, cmb)
        | _ -> failwith "expected elements and combinations")
    |> fun (els, cmb) ->
            (Str.split (Str.regexp ", ") els,  String.split_on_char '\n' cmb)
    |> fun (els, cmb) ->
            let reduced = reduce_set els in
            List.filter (fun el -> is_possible el reduced) cmb
    |> List.length

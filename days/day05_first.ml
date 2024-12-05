let list_contains list el =
    match List.find_index (fun a -> a == el) list with
    | Some(_) -> 1
    | None -> 0

let solve filename =
    filename
    |> Input_reader.read_input
    |> String.concat "\n"
    |> Str.split (Str.regexp "\n\n")
    |> (function
        | [rules; pages] -> (rules, pages)
        | _ -> failwith "Expected rules and pages")
    |> fun (rules, pages) ->
            let
            processed_pages =
                pages
                |> String.split_on_char '\n'
                |> List.map (fun els ->
                        els
                        |> String.split_on_char ','
                        |> List.map int_of_string
                )
            in

            let processed_rules =
                rules
                |> String.split_on_char '\n'
                |> List.map (fun el ->
                    el
                    |> String.split_on_char '|'
                    |> List.map int_of_string
                    |> (function
                        | [before; after] -> (before, after)
                        | _ -> failwith "Expected before and after values"
                    )
                )
                |> List.fold_left (fun acc (before, after) ->
                        try
                            let current_list = List.assoc before acc in
                            (before, after :: current_list) :: List.remove_assoc before acc
                        with
                            Not_found -> (before, [after]) :: acc
                    ) []
            in
            (processed_rules, processed_pages)
    |> fun (rules, pages) ->
            List.filter (fun pages -> 
                let
                sorted = (List.sort (fun p1 p2 -> list_contains (try List.assoc p2 rules with Not_found -> []) p1) pages)
                in
                pages = sorted
            ) pages
    |> fun filtered_pages ->
            List.fold_left (fun acc filtered ->
                (List.nth filtered (List.length filtered / 2)) + acc
            ) 0 filtered_pages

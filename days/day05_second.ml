open Day05_shared

let solve filename =
    (* Reads input, splits into the two parts. *)
    filename
    |> Input_reader.read_input
    |> String.concat "\n"
    |> Str.split (Str.regexp "\n\n")
    |> (function
        | [rules; pages] -> (rules, pages)
        | _ -> failwith "Expected rules and pages")
    (* Converts into usable format. *)
    |> fun (rules, pages) ->
            (process_rules rules, process_pages pages)
    (* Filters based on wheter the sorted list is the same or not as the original
       (we only want the unsorted ones)
       and sums up their middle element (of the sorted version). *)
    |> fun (rules, pages) ->
            List.fold_left (fun acc page ->
                let sorted = (List.sort (fun p1 p2 -> list_contains (try List.assoc p2 rules with Not_found -> []) p1) page) in
                if page = sorted
                then acc else
                (List.nth sorted (List.length sorted / 2)) + acc
            ) 0 pages

(** A simple helper that just checks whether an element is present in a list.
    Returns int as it is meant to be used as a predicate for sorting *)
let list_contains list el =
    match List.find_index (fun a -> a == el) list with
    | Some(_) -> 1
    | None -> 0

(** Parses the pages from the input as described int the puzzle.
    Basically just parse a int list list from text.*)
let process_pages pages = 
    pages
    |> String.split_on_char '\n'
    |> List.map (fun els ->
            els
            |> String.split_on_char ','
            |> List.map int_of_string
    )

(** Parses the rules from the input into a associative list of lists,
    which represents the relationships between the pages. *)
let process_rules rules =
    rules
    (* Parses the text into int * int list. *)
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
    (* Creates the associations between pages. *)
    |> List.fold_left (fun acc (before, after) ->
            try
                let current_list = List.assoc before acc in
                (before, after :: current_list) :: List.remove_assoc before acc
            with
                Not_found -> (before, [after]) :: acc
        ) []

(**
   Creates a list of differences between subsequent elements of a list.
*)
let rec diffs = function
    | [] | [_] -> []
    | a :: b :: tail -> (b - a) :: diffs (b :: tail)

(**
    Filters based on parameters defined in the description.
    (1) -> all differences must have a magnitude in the interval <1..3>.
    (2) -> all of the differences must have the same sign (+/-).
*)
let is_safe diffs =
    not (
        (List.exists (fun el -> el < 0) diffs &&
        List.exists (fun el -> el > 0) diffs) || 
        (List.exists (fun el -> (abs el) > 3 || (abs el) < 1 ) diffs)
    )


open Day03_shared

(** Filters out parts of the string that are 'enabled' as described in the puzzle based on the do() and don't() instructions. *)
let take_enabled (str : string) =
    (* Keep track of the input, accumulated output, accumulated instruction token and current mode. *)
    let rec aux (inp : string) (acc : string) (tacc : string) (enabled : bool) =
        match tacc with
        (* Switch modes based on instruction token. *)
        | "do()" -> aux inp acc "" true
        | "don't()" -> aux inp acc "" false
        | _ -> (
            (* If empty list, return output accumulator. *)
            match List.of_seq (String.to_seq inp) with
            | [] -> acc
            | a :: tail -> 
            let
                (* Update instruction token accumulator if the current one plus the current character are a prefix of one of the possible tokens. *)
                tacc =
                if
                    String.starts_with ~prefix:(tacc ^ (String.make 1 a)) "do()" 
                    || String.starts_with ~prefix:(tacc ^ (String.make 1 a)) "don't()" 
                then
                    (tacc ^ (String.make 1 a))
                else
                    ""
            in
            (* If enabled prepend the current character to the accumulator and continue to the next character. *)
            if enabled then
                aux (tail |> List.to_seq |> String.of_seq) (acc ^ (String.make 1 a)) tacc enabled
            (* Else just continue to the next character. *)
            else
                aux (tail |> List.to_seq |> String.of_seq) acc tacc enabled
        )

    in aux str "" "" true



(**
    Solves the puzzle for day 1 part 2, loading input from given filename.
*)
let solve filename =
    Input_reader.read_input filename
    |> List.hd
    (* Filter enabled. *)
    |> take_enabled
    |> Str.split (Str.regexp "mul")
    (* Gather products. *)
    |> List.map (fun expr -> filter expr)
    (* Sum products. *)
    |> List.fold_left (+) 0

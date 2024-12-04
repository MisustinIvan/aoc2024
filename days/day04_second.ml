open Day04_shared

(** Combines 3 character into a word. *)
let combine_word a b c =
    String.make 1 a ^ String.make 1 b ^ String.make 1 c

(** Count number of occurences of "XMAS" in input list. *)
let count_words words =
    List.length (List.filter (fun word -> (word = "MAS" || word = "SAM")) words)

(**
    Solves the puzzle for day 4 part 1, loading input from given filename.
*)
let solve filename =
    filename
    |> Input_reader.read_input
    |> List.map (fun ln -> ln |> String.to_seq |> Array.of_seq )
    |> Array.of_list
    |> fun table ->
        Array.mapi (fun ridx row ->
            Array.mapi (fun cidx _ ->
                (* We just worry about 2 diagonals. *)
                let d1 = combine_word
                    (get_el table (ridx-1) (cidx-1))
                    (get_el table ridx cidx)
                    (get_el table (ridx+1) (cidx+1))
                in
                let d2 = combine_word
                    (get_el table (ridx+1) (cidx-1))
                    (get_el table ridx cidx)
                    (get_el table (ridx-1) (cidx+1))
                in

                if count_words [d1; d2] == 2 then 1 else 0
            ) row
        ) table
    |> Array.fold_left (fun acc row -> acc + Array.fold_left (+) 0 row) 0

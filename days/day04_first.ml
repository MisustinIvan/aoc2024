open Day04_shared

(** Combines 4 character into a word. *)
let combine_word a b c d =
    String.make 1 a ^ String.make 1 b ^ String.make 1 c ^ String.make 1 d

(** Count number of occurences of "XMAS" in input list. *)
let count_words words =
    List.length (List.filter (fun word -> word = "XMAS") words)

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
                (* Now get all the possible options -> 4 straight directions and 4 diagonals. *)
                let north = combine_word 
                    (get_el table ridx cidx)
                    (get_el table (ridx+1) cidx)
                    (get_el table (ridx+2) cidx)
                    (get_el table (ridx+3) cidx)
                in
                let south = combine_word
                    (get_el table ridx cidx)
                    (get_el table (ridx-1) cidx)
                    (get_el table (ridx-2) cidx)
                    (get_el table (ridx-3) cidx)
                in
                let east = combine_word
                    (get_el table ridx cidx)
                    (get_el table ridx (cidx+1))
                    (get_el table ridx (cidx+2))
                    (get_el table ridx (cidx+3))
                in
                let west = combine_word
                    (get_el table ridx cidx)
                    (get_el table ridx (cidx-1))
                    (get_el table ridx (cidx-2))
                    (get_el table ridx (cidx-3))
                in

                let north_east = combine_word
                    (get_el table ridx cidx)
                    (get_el table (ridx+1) (cidx+1))
                    (get_el table (ridx+2) (cidx+2))
                    (get_el table (ridx+3) (cidx+3))
                in
                let north_west = combine_word
                    (get_el table ridx cidx)
                    (get_el table (ridx+1) (cidx-1))
                    (get_el table (ridx+2) (cidx-2))
                    (get_el table (ridx+3) (cidx-3))
                in
                let south_east = combine_word
                    (get_el table ridx cidx)
                    (get_el table (ridx-1) (cidx+1))
                    (get_el table (ridx-2) (cidx+2))
                    (get_el table (ridx-3) (cidx+3))
                in
                let south_west = combine_word
                    (get_el table ridx cidx)
                    (get_el table (ridx-1) (cidx-1))
                    (get_el table (ridx-2) (cidx-2))
                    (get_el table (ridx-3) (cidx-3))
                in

                count_words [north; south; east; west; north_east; north_west; south_east; south_west]
            ) row
        ) table
    |> Array.fold_left (fun acc row -> acc + Array.fold_left (+) 0 row) 0

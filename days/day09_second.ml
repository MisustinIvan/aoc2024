open Day09_shared

(** Finds the first open position and returns the elements before and after it *)
let first_open_pos (lst : (int * int) list) (sz : int) =
  let rec aux acc lst =
    match lst with
    | (hid, hsz) :: tail ->
        if hid = -1 && hsz >= sz then
          (List.rev acc, Some (hid, hsz), tail)
        else
          aux ((hid, hsz) :: acc) tail
    | [] -> (List.rev acc, None, [])
  in
  aux [] lst

(**
    Solves the puzzle for day 9 part 1, loading input from given filename.
*)
let solve filename = filename
    |> Input_reader.read_input
    |> (function
        | [list] -> list
        | _ -> failwith "Expected a single line of text.")
    |> String.to_seq
    |> List.of_seq
    |> List.map (fun el -> String.make 1 el |> int_of_string)
    |> fun nums ->
            let rec aux acc nums id mode =
                match nums with
                | head :: tail -> if mode
                        then aux ((id, head) :: acc) tail (id+1) false
                        else aux ((-1, head) :: acc) tail (id) true
                | [] -> List.rev acc
            in aux [] nums 0 true
    (* Defragment. *)
    |> fun els ->
            let rec aux (acc : (int*int) list) (els : (int*int) list) =
                match els with
                | (hid, hsz) :: tail ->
                        if hid = -1
                        then
                            aux ((hid, hsz) :: acc) tail
                        else
                            let (bef, x, aft) = first_open_pos (List.rev tail) hsz in
                            (
                            match x with
                            | None -> aux ((hid, hsz) :: acc) tail
                            | Some (xid, xsz) ->
                                    if xsz > hsz
                                    then
                                        aux ((xid, hsz) :: acc) (List.append (List.rev aft) ((xid, xsz-hsz) :: (hid, hsz) :: (List.rev bef)))
                                    else
                                        aux ((xid, xsz) :: acc) (List.append (List.rev aft) ((hid, hsz) :: (List.rev bef)))
                            )
                | [] -> acc
            in aux [] (List.rev els)
    |> List.map (fun (id, sz) -> repeat_a id sz)
    |> List.flatten
    |> List.mapi (fun i id -> if id != -1 then i*id else 0)
    |> List.fold_left (+) 0

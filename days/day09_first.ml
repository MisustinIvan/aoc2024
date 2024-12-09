(** Repeats a given string n times. *)
let repeat_str str n =
    let rec aux acc n =
        match n with
        | 0 -> acc
        | _ -> aux (acc ^ str) (n-1)
    in aux "" n

let repeat_a a n =
    let rec aux acc n =
        match n with
        | 0 -> acc
        | _ -> aux (a :: acc) (n-1)
    in aux [] n

let find_index_from_end arr elem =
    let len = Array.length arr in
    let rec aux i acc = if i < 0
        then acc
        else if arr.(i) = elem then Some (len - 1 - i)
        else aux (i - 1) acc
    in
      aux (len - 1) None

(** Find the first positive element in the list, returns it and the elements behind it. *)
let first_pos_and_rest lst =
  let rec aux acc lst =
    match lst with
    | head :: tail ->
        if head > 0 then
          (List.rev acc, Some head, tail)
        else
          aux (head :: acc) tail
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
                        then aux ((repeat_a id head) :: acc) tail (id+1) false
                        else aux ((repeat_a (-1) head) :: acc) tail (id) true
                | [] -> List.concat (List.rev acc)
            in aux [] nums 0 true
    (* Defragment. *)
    |> fun nums ->
            let rec aux (fs : int list) (bs : int list) (fn : int list) =
                match fn with
                | head :: tail ->
                        if head = -1
                        then 
                            (* Get first positive element from the back. *)
                            let (negs, a, rest) = (first_pos_and_rest (List.rev tail)) in
                            match a with
                            (* If finds a positive number. *)
                            | Some(x) -> aux (x :: fs) (head :: (List.append bs negs)) (List.rev rest)
                            (* If only negative numbers. *)
                            | None -> List.append (List.rev fs) (head :: bs)
                        else
                            aux (head :: fs) (bs) (tail) (* This case is fine. *)
                | [] -> List.append (List.rev fs) bs (* This case is also fine. *)
            in aux [] [] nums
    |> List.mapi (fun i el -> if el = -1 then 0 else (i*el))
    |> List.fold_left (+) 0

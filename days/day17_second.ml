open Day17_shared

let parse_prog input = 
    input
    |> String.split_on_char '\n'
    |> List.rev
    |> List.hd
    |> String.split_on_char ' '
    |> List.rev
    |> List.hd
    |> String.split_on_char ','

let solve filename =
    let input = Input_reader.read_input filename |> String.concat "\n" in
    let machine, program = parse_input input, parse_prog input in
    (* the output we are trying to reach *)
    let target = Array.of_list (List.rev program) in

    let rec aux a depth =
        (* max search depth *)
        if depth = Array.length target then Some a
        else
            (* tries 8 sections of outputs *)
            let rec try_digits i =
            if i = 8 then None else
                (* runs vm for trial value of a (shifted a to the left and added i) (ocatal repr) *)
                let res = VirtualMachine.run { machine with a = a * 8 + i } in
                let out = Array.of_list (List.rev res.out) in
                (* check if corresponding value at given depth *)
                if out.(0) = target.(depth) then
                    (* check next branch *)
                    match aux (a * 8 + i) (depth + 1) with
                    (* if finds value, propagate *)
                    | Some x -> Some x
                    (* if does not find, try another value of i *)
                    | None -> try_digits (i + 1)
                else
                    try_digits (i + 1)
            in
            try_digits 0
    in
    Option.value (aux 0 0) ~default:0

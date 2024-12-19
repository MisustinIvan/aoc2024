let count_ways prefixes target =
    let n = String.length target in
    let cache = Array.make (n + 1) 0 in
    cache.(0) <- 1;

    for i = 1 to n do
        List.iter (fun prefix ->
            let prefix_len = String.length prefix in
        if i >= prefix_len then
            let substring = String.sub target (i - prefix_len) prefix_len in
            if substring = prefix then
                cache.(i) <- cache.(i) + cache.(i - prefix_len)
        ) prefixes
    done;
    cache.(n)

let solve filename =
    filename
    |> Input_reader.read_input
    |> String.concat "\n"
    |> Str.split (Str.regexp "\n\n")
    |> (function
        | [els; cmb] -> (els, cmb)
        | _ -> failwith "expected elements and combinations")
    |> fun (els, cmb) ->
            (Str.split (Str.regexp ", ") els,  String.split_on_char '\n' cmb)
    |> fun (els, cmb) ->
            List.fold_left (fun acc target -> acc + count_ways els target) 0 cmb

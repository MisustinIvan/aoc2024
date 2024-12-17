open Day17_shared

let solve filename =
    filename
    |> Input_reader.read_input
    |> String.concat "\n"
    |> parse_input
    |> VirtualMachine.run
    |> fun state ->
        (String.concat "," (List.rev state.out))

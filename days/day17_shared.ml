module VirtualMachine = struct
    type instruction =
        | Adv
        | Bxl
        | Bst
        | Jnz
        | Bxc
        | Out
        | Bdv
        | Cdv

    let parse_instruction i =
        match i with
        | '0' -> Adv
        | '1' -> Bxl
        | '2' -> Bst
        | '3' -> Jnz
        | '4' -> Bxc
        | '5' -> Out
        | '6' -> Bdv
        | '7' -> Cdv
        | ui -> failwith (Printf.sprintf "unexpected instruction: %c; can't parse" ui)

    type combo_operand =
        | Lit0
        | Lit1
        | Lit2
        | Lit3
        | RegA
        | RegB
        | RegC
        | Reserved

    let parse_combo_operand o =
        match o with
        | '0' -> Lit0
        | '1' -> Lit1
        | '2' -> Lit2
        | '3' -> Lit3
        | '4' -> RegA
        | '5' -> RegB
        | '6' -> RegC
        | '7' -> Reserved
        | uo -> failwith (Printf.sprintf "unexpected operand: %c; can't parse" uo)

    type literal_operand = int

    let parse_literal_operand o =
        int_of_string (String.make 1 o)

    type operand =
        | Operand of combo_operand
        | Literal of literal_operand

    let op_as_combo (o : operand) =
        match o with
        | Operand combo_operand -> combo_operand
        | _ -> failwith "failed to extract, expected combo operand"

    let op_as_literal (o : operand) =
        match o with
        | Literal literal_operand -> literal_operand
        | _ -> failwith "failed to extract, expected literal operand"

    type io = {
        instruction: instruction;
        operand: operand;
    }

    type machine_state = {
        a: int;
        b: int;
        c: int;
        pc: int;
        mem: io array;
        out: string list;
    }

    let show_combo_operand (machine : machine_state) (o : combo_operand) =
        match o with
        | Lit0 -> "Lit0"
        | Lit1 -> "Lit1"
        | Lit2 -> "Lit2"
        | Lit3 -> "Lit3"
        | RegA -> Printf.sprintf "RegA: %d" machine.a
        | RegB -> Printf.sprintf "RegB: %d" machine.b
        | RegC -> Printf.sprintf "RegC: %d" machine.c
        | Reserved -> "Reserved"

    let show_literal_operand (o : literal_operand) =
        "Lit op: " ^ string_of_int o

    let show_operand (machine : machine_state) (o : operand) =
        match o with
        | Operand combo_operand -> show_combo_operand machine combo_operand
        | Literal literal_operand -> show_literal_operand literal_operand

    let combo_op_to_value machine (operand : combo_operand) =
        match operand with
        | Lit0 -> 0
        | Lit1 -> 1
        | Lit2 -> 2
        | Lit3 -> 3
        | RegA -> machine.a
        | RegB -> machine.b
        | RegC -> machine.c
        | Reserved -> failwith "unexpected reserved operand"

    let execute_adv machine (operand : combo_operand) =
        { machine with
            a = machine.a / int_of_float (2.0 ** float_of_int (combo_op_to_value machine operand));
            pc = machine.pc + 1
        }

    let execute_bxl machine (operand : literal_operand) =
        { machine with
            b = machine.b lxor operand;
            pc = machine.pc + 1
        }

    let execute_bst machine (operand : combo_operand) =
        { machine with
            b = (combo_op_to_value machine operand) mod 8;
            pc = machine.pc + 1
        }

    let execute_jnz machine (operand : literal_operand) =
        if machine.a = 0
        then
            { machine with
                pc = machine.pc + 1
            }
        else
            { machine with
                pc = operand
            }

    let execute_bxc machine _ =
        { machine with
            b = machine.b lxor machine.c;
            pc = machine.pc + 1
        }

    let execute_out machine (operand : combo_operand) =
        { machine with
            out = (string_of_int ((combo_op_to_value machine operand) mod 8)) :: machine.out;
            pc = machine.pc + 1
        }

    let execute_bdv machine (operand : combo_operand) =
        { machine with
            b = machine.a / int_of_float (2.0 ** float_of_int (combo_op_to_value machine operand));
            pc = machine.pc + 1
        }

    let execute_cdv machine (operand : combo_operand) =
        { machine with
            c = machine.a / int_of_float (2.0 ** float_of_int (combo_op_to_value machine operand));
            pc = machine.pc + 1
        }

    let execute_instruction machine (io : io) = 
        let instruction, operand = io.instruction, io.operand in
        match instruction with
        | Adv -> execute_adv machine (op_as_combo operand)
        | Bxl -> execute_bxl machine (op_as_literal operand)
        | Bst -> execute_bst machine (op_as_combo operand)
        | Jnz -> execute_jnz machine (op_as_literal operand)
        | Bxc -> execute_bxc machine operand
        | Out -> execute_out machine (op_as_combo operand)
        | Bdv -> execute_bdv machine (op_as_combo operand)
        | Cdv -> execute_cdv machine (op_as_combo operand)

    let run machine =
        let program_length = Array.length machine.mem in
        let rec aux (state : machine_state) =
            if state.pc >= program_length then state
            else aux (execute_instruction state state.mem.(state.pc))
        in
        aux machine
end

let parse_input (input : string) =
    let regex = Str.regexp "Register A: \\([0-9]+\\)\nRegister B: \\([0-9]+\\)\nRegister C: \\([0-9]+\\)\n\nProgram: \\([0-9,]+\\)" in
    if Str.string_match regex input 0 then
        let rega = int_of_string (Str.matched_group 1 input) in
        let regb = int_of_string (Str.matched_group 2 input) in
        let regc = int_of_string (Str.matched_group 3 input) in
        let mem = (Str.matched_group 4 input) |> String.to_seq |> List.of_seq |> List.filter (fun el -> el <> ',') in
        let rec parse_mem (acc : VirtualMachine.io list) (mem : char list) =
            match mem with
            | i :: o :: rest -> parse_mem (
                    let instruction = VirtualMachine.parse_instruction i in
                    ({
                        instruction = (VirtualMachine.parse_instruction i);
                        operand = (match instruction with
                        | Adv | Bst | Out | Bdv | Cdv -> Operand (VirtualMachine.parse_combo_operand o)
                        | Bxl | Jnz | Bxc -> Literal (VirtualMachine.parse_literal_operand o));
                    } :: acc)
                )
                rest
            | [] | [_] -> Array.of_list (List.rev acc)
        in VirtualMachine.{a = rega; b = regb; c = regc; pc = 0; mem = (parse_mem [] mem); out = []}
    else
        failwith "invalid input, can't parse"

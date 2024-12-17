open Common.Functions
open System

type Program = list<int>

type State =
    { A: int
      B: int
      C: int
      InstructionPointer: int }

let readInput filename =
    let afterSemicolon (line: string) = line.Substring(line.IndexOf(":") + 2)

    let parseProgram (programStr: string) =
        programStr |> _.Split(',') |> Array.toList |> List.map int

    let lines = readlines filename

    { A = afterSemicolon lines[0] |> int
      B = afterSemicolon lines[1] |> int
      C = afterSemicolon lines[2] |> int
      InstructionPointer = 0 },
    afterSemicolon lines[4] |> parseProgram

let evaluateCombo state operand =
    if operand >= 0 && operand <= 3 then
        operand
    else if operand = 4 then
        state.A
    else if operand = 5 then
        state.B
    else if operand = 6 then
        state.C
    else
        raise (ArgumentException(sprintf "Invalid operand %i" operand))

let adv state operand =
    let combo = evaluateCombo state operand

    { state with
        A = state.A / (pown 2 combo)
        InstructionPointer = state.InstructionPointer + 2 }

let bxl state operand =
    { state with
        B = state.B ^^^ operand
        InstructionPointer = state.InstructionPointer + 2 }

let bst state operand =
    let combo = evaluateCombo state operand

    { state with
        B = combo % 8
        InstructionPointer = state.InstructionPointer + 2 }

let jnz state operand =
    if state.A = 0 then
        { state with
            InstructionPointer = state.InstructionPointer + 2 }
    else
        { state with
            InstructionPointer = operand }

let bxc state =
    { state with
        B = state.B ^^^ state.C
        InstructionPointer = state.InstructionPointer + 2 }

let out state operand =
    let combo = evaluateCombo state operand

    { state with
        InstructionPointer = state.InstructionPointer + 2 },
    combo % 8

let bdv state operand =
    let combo = evaluateCombo state operand

    { state with
        B = state.A / (pown 2 combo)
        InstructionPointer = state.InstructionPointer + 2 }

let cdv state operand =
    let combo = evaluateCombo state operand

    { state with
        C = state.A / (pown 2 combo)
        InstructionPointer = state.InstructionPointer + 2 }


let evaluate (startState: State) (program: Program) =
    let rec execute (state: State) =
        if state.InstructionPointer >= program.Length then
            []
        else
            let opcode, operand =
                program[state.InstructionPointer], program[state.InstructionPointer + 1]

            if opcode = 5 then
                let nextState, outValue = out state operand
                [ outValue ] @ execute nextState
            else
                let nextState =
                    match opcode with
                    | 0 -> adv state operand
                    | 1 -> bxl state operand
                    | 2 -> bst state operand
                    | 3 -> jnz state operand
                    | 4 -> bxc state // bxc does not take an operand argument
                    // skip 5
                    | 6 -> bdv state operand
                    | 7 -> cdv state operand
                    | _ -> raise (ArgumentException(sprintf "Unknown opcode %i" opcode))

                execute nextState

    execute startState

let findSmallestA state program =
    let rec helper a uniqueResults =
        if a % 100000 = 0
        then printfn "%i, num unique results: %i" a (Set.count uniqueResults)

        let output = evaluate {state with A = a} program
        if output = program
        then a
        else helper (a + 1) (Set.add output uniqueResults)
    
    helper 1 Set.empty

let startState, program = readInput "input.dat"
// 207303137 incorrect
evaluate startState program
|> List.map string
|> String.concat ","
|> part1

findSmallestA startState program |> part2
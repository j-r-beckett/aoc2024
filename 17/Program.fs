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

let rec advanceState (state: State) (program: Program) =
    if state.InstructionPointer >= program.Length then
        None
    else
        let opcode, operand =
            program[state.InstructionPointer], program[state.InstructionPointer + 1]

        if opcode = 5 then
            let nextState, outValue = out state operand
            Some (nextState, Some outValue)
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

            advanceState nextState program

let rec evaluate (startState: State) (program: Program) =
    match advanceState startState program with
    | None -> []
    | Some (nextState, outputOption) ->
        match outputOption with
        | None -> evaluate nextState program
        | Some output -> [output] @ evaluate nextState program

let rec producesTargetOutput (startState: State) (program: Program) (targetOutput: list<int>) =
    match advanceState startState program with
    | None -> targetOutput.IsEmpty
    | Some (nextState, outputOption) ->
        match outputOption with
        | None -> targetOutput.IsEmpty
        | Some output ->  
            if targetOutput.Length > 0 && output = targetOutput[0]
            then producesTargetOutput nextState program targetOutput[1..]
            else false

let findSmallestA state program =
    let rec helper a  =
        if a % 1000000 = 0
        then printfn "%i, %A" a (evaluate {state with A = a} program)

        if producesTargetOutput {state with A = a} program program
        then a
        else helper (a + 1)
    
    helper 1

let rec prettyPrint (program: list<int>) =
    if program.IsEmpty
    then []
    else 
        let instruction = match program[0] with
                                | 0 -> "adv"
                                | 1 -> "bxl"
                                | 2 -> "bst"
                                | 3 -> "jnz"
                                | 4 -> "bxc"
                                | 5 -> "out"
                                | 6 -> "bdv"
                                | 7 -> "cdv"
                                | _ -> raise (ArgumentException(sprintf "Unknown instruction %i" program[0]))
        let operand = match program[1] with
                            | n when n >= 0 && n <= 3 -> string n
                            | 4 -> "A"
                            | 5 -> "B"
                            | 6 -> "C"
                            | _ -> raise (ArgumentException(sprintf "Unknown operand %i" program[1]))
        [sprintf "%s %s" instruction operand] @ prettyPrint program[2..]


let startState, program = readInput "input.dat"
// 207303137 incorrect
evaluate startState program
|> List.map string
|> String.concat ","
|> part1

prettyPrint program |> String.concat "\n" |> printfn "%s"
// findSmallestA startState program |> part2
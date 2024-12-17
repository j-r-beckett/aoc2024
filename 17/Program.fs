open Common.Functions
open System

type Program = list<int64>

type State =
    { A: int64
      B: int64
      C: int64
      InstructionPointer: int }

let readInput filename =
    let afterSemicolon (line: string) = line.Substring(line.IndexOf(":") + 2)

    let parseProgram (programStr: string) =
        programStr |> _.Split(',') |> Array.toList |> List.map int64

    let lines = readlines filename

    { A = afterSemicolon lines[0] |> int64
      B = afterSemicolon lines[1] |> int64
      C = afterSemicolon lines[2] |> int64
      InstructionPointer = 0 },
    afterSemicolon lines[4] |> parseProgram

let evaluateCombo state operand =
    if operand >= 0L && operand <= 3L then
        operand
    else if operand = 4L then
        state.A
    else if operand = 5L then
        state.B
    else if operand = 6L then
        state.C
    else
        raise (ArgumentException(sprintf "Invalid operand %i" operand))

let adv state operand =
    let combo = evaluateCombo state operand

    { state with
        A = state.A / (pown 2L (int combo))
        InstructionPointer = state.InstructionPointer + 2 }

let bxl state operand =
    { state with
        B = state.B ^^^ operand
        InstructionPointer = state.InstructionPointer + 2 }

let bst state (operand: int64) =
    let combo = evaluateCombo state operand

    { state with
        B = combo % 8L
        InstructionPointer = state.InstructionPointer + 2 }

let jnz state operand =
    if state.A = 0L then
        { state with
            InstructionPointer = state.InstructionPointer + 2 }
    else
        { state with
            InstructionPointer = int operand }

let bxc state =
    { state with
        B = state.B ^^^ state.C
        InstructionPointer = state.InstructionPointer + 2 }

let out state operand =
    let combo = evaluateCombo state operand

    { state with
        InstructionPointer = state.InstructionPointer + 2 },
    combo % 8L

let bdv state operand =
    let combo = evaluateCombo state operand

    { state with
        B = state.A / (pown 2L (int combo))
        InstructionPointer = state.InstructionPointer + 2 }

let cdv state operand =
    let combo = evaluateCombo state operand

    { state with
        C = state.A / (pown 2L (int combo))
        InstructionPointer = state.InstructionPointer + 2 }

let rec advanceState (state: State) (program: Program) =
    if state.InstructionPointer >= program.Length then
        None
    else
        let opcode, operand =
            program[state.InstructionPointer], program[state.InstructionPointer + 1]

        if opcode = 5 then
            let nextState, outValue = out state operand
            Some(nextState, Some outValue)
        else
            let nextState =
                match opcode with
                | 0L -> adv state operand
                | 1L -> bxl state operand
                | 2L -> bst state operand
                | 3L -> jnz state operand
                | 4L -> bxc state // bxc does not take an operand argument
                // skip 5
                | 6L -> bdv state operand
                | 7L -> cdv state operand
                | _ -> raise (ArgumentException(sprintf "Unknown opcode %i" opcode))

            advanceState nextState program

let rec evaluate (startState: State) (program: Program) =
    match advanceState startState program with
    | None -> []
    | Some(nextState, outputOption) ->
        match outputOption with
        | None -> evaluate nextState program
        | Some output -> [ output ] @ evaluate nextState program

let rec producesTargetOutput (startState: State) (program: Program) (targetOutput: list<int>) =
    match advanceState startState program with
    | None -> targetOutput.IsEmpty
    | Some(nextState, outputOption) ->
        match outputOption with
        | None -> targetOutput.IsEmpty
        | Some output ->
            if targetOutput.Length > 0 && output = targetOutput[0] then
                producesTargetOutput nextState program targetOutput[1..]
            else
                false


let rec first fn lst =
    match lst with
    | [] -> raise (ArgumentException("No first found"))
    | head :: tail -> if fn head then head else first fn tail


let octal (n: int64) = System.Convert.ToString(n, 8)


let findSmallestA state program =
    let helper (aSoFar: int64) (rOutputIndex: int) =
        [ for n in [ 0L .. 7L ] -> n * (pown 8L rOutputIndex) + aSoFar ]
        |> List.map (fun a -> a, evaluate { state with A = a } program)
        // |> List.filter (fun (_, output) -> output = program[program.Length - rOutputIndex - 1 ..])
        |> List.map (fun (a, output) -> octal a, output, program[program.Length - rOutputIndex - 1 ..])
        |> printfn "%A"

        [ for n in [ 0L .. 7L ] -> n * (pown 8L rOutputIndex) + aSoFar ]
        |> List.map (fun a -> a, evaluate { state with A = a } program)
        |> List.filter (fun (_, output) -> output = program[program.Length - rOutputIndex - 1 ..])
        |> List.head
        |> fst

    List.fold (fun aSoFar index -> helper aSoFar index) 0 [ 0 .. program.Length - 1 ]
// List.fold ()

// let findSmallestA state program =
//     let rec helper a  =
//         if a % 1000000 = 0
//         then printfn "%i, %A" a (evaluate {state with A = a} program)

//         if producesTargetOutput {state with A = a} program program
//         then a
//         else helper (a + 1)

//     helper 1

let rec prettyPrint (program: list<int64>) =
    if program.IsEmpty then
        []
    else
        let getOperand n combo =
            if combo then
                match n with
                | _ when n >= 0L && n <= 3L -> string n
                | 4L -> "A"
                | 5L -> "B"
                | 6L -> "C"
                | _ -> raise (ArgumentException(sprintf "Unknown operand %i" program[1]))
            else
                string n

        let instruction, isCombo =
            match program[0] with
            | 0L -> "adv", true
            | 1L -> "bxl", false
            | 2L -> "bst", true
            | 3L -> "jnz", false
            | 4L -> "bxc", false
            | 5L -> "out", true
            | 6L -> "bdv", true
            | 7L -> "cdv", true
            | _ -> raise (ArgumentException(sprintf "Unknown instruction %i" program[0]))

        [ sprintf "%s %s" instruction (getOperand program[1] isCombo) ]
        @ prettyPrint program[2..]


let startState, program = readInput "test.dat"
// 207303137 incorrect
evaluate startState program |> List.map string |> String.concat "," |> part1

prettyPrint program |> String.concat "\n" |> printfn "%s"
// findSmallestA startState program |> part2
// program |> printfn "target program: %A"
// evaluate {startState with A = 1000000002} program |> printfn "%A"
// let outputs = [for a in [0..40000] -> evaluate {startState with A = a} program]
// [for i in [0..outputs.Length - 2] -> (i, outputs[i], outputs[i + 1])] |> List.filter (fun (_, first, second) -> first.Length + 1 = second.Length) |> List.map (fun (index, first, _) -> (first.Length, index)) |> printfn "%A"

for a in [ 0L .. 1200440L ] do
    let output = evaluate { startState with A = a } program

    if program[program.Length - output.Length ..] = output then
        printfn "A: %s, output: %A" (System.Convert.ToString(a, 8)) output

// evaluate { startState with A = 117440 } program |> printfn "%A"
findSmallestA startState program |> printfn "%A"

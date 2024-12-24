open Common.Functions
open System.Text.RegularExpressions
open System

type Operator =
    | AND
    | OR
    | XOR

type Operation =
    { Input1: string
      Operator: Operator
      Input2: string
      Output: string }

let str2Operator s =
    match s with
    | "AND" -> AND
    | "OR" -> OR
    | "XOR" -> XOR
    | _ -> raise (System.ArgumentException "Unknown operator")

let evalOperator input1 input2 operator =
    match operator with
    | AND -> input1 && input2
    | OR -> input1 || input2
    | XOR -> (input1 && (not input2)) || (input2 && (not input1))

let parseInput filename =
    let parseWire (wireStr: string) =
        wireStr[.. wireStr.IndexOf ':' - 1], wireStr[wireStr.IndexOf ':' + 2] = '1'

    let parseOperation (opStr: string) =
        let pattern = @"(.{3}) (AND|OR|XOR) (.{3}) -> (.{3})"
        let groups = Regex.Match(opStr, pattern) |> _.Groups |> Seq.map string |> Seq.toList

        { Input1 = groups[1]
          Operator = str2Operator groups[2]
          Input2 = groups[3]
          Output = groups[4] }

    let lines = readlines filename
    let boundary = List.findIndex ((=) "") lines
    let wires = lines[.. boundary - 1] |> List.map parseWire |> Map.ofList
    let operations = lines[boundary + 1 ..] |> List.map parseOperation
    wires, operations

let rec applyOperations (wires: Map<string, bool>) (operations: list<Operation>) =
    let applyOperation (operation: Operation) =
        Map.add operation.Output (evalOperator wires[operation.Input1] wires[operation.Input2] operation.Operator) wires

    match operations with
    | [] -> wires
    | operation :: remaining ->
        if Map.containsKey operation.Input1 wires && Map.containsKey operation.Input2 wires then
            applyOperations (applyOperation operation) remaining
        else
            applyOperations wires (remaining @ [ operation ])

let decodeNumber (gateLetter: char) (wires: Map<string, bool>) =
    let s =
        wires
        |> Map.toList
        |> List.filter (fun (wireName, _) -> wireName[0] = gateLetter)
        |> List.sort
        |> List.rev
        |> List.map snd
        |> List.map (fun b -> if b then "1" else "0")
        |> String.concat ""

    Convert.ToInt64(s, 2)

let countDiffDigits (actual: int64) (expected: int64) =
    let toBinaryList n = sprintf "%46B" n |> Seq.toList

    List.zip (toBinaryList actual) (toBinaryList expected)
    |> List.filter (fun (a, b) -> a <> b)
    |> List.length


let prettyPrintCircuit (operations: list<Operation>) =
    let map = operations |> List.map (fun op -> op.Output, op) |> Map.ofList

    let op2Str (op: Operation) =
        sprintf "%A %A %A -> %A" op.Input1 op.Operator op.Input2 op.Output

    let rec prettyPrintOperation (operation: Operation) (indentLevel: int) =
        let indent = [ for _ in [ 0 .. indentLevel - 1 ] -> "   " ] |> String.concat ""
        printfn "%s%s" indent (op2Str operation)

        if operation.Input1[0] <> 'x' && operation.Input1[0] <> 'y' then
            prettyPrintOperation map[operation.Input1] (indentLevel + 1)

        if operation.Input2[0] <> 'x' && operation.Input2[0] <> 'y' then
            prettyPrintOperation map[operation.Input2] (indentLevel + 1)

    for op in operations do
        if op.Output[0] = 'z' then
            prettyPrintOperation op 0


// let findSusOperations (operations: list<Operation>) =
//     let

type OperationTree =
    | BaseOp of input1: string * operator: Operator * input2: string * output: string
    | IntermediateOp of input1: OperationTree * operator: Operation * input2: OperationTree

type OperationTreeNode =
    | BaseInput of wireName: string
    | IntermediateInput of input1: OperationTreeNode * operator: Operator * input2: OperationTreeNode

let isBaseInput (input: string) = input[0] = 'x' || input[0] = 'y'

let rec decompose (map: Map<string, Operation>) (operation: Operation) =
    let input1 =
        if isBaseInput operation.Input1 then
            BaseInput(operation.Input1)
        else
            IntermediateInput(decompose map map[operation.Input1])

    let input2 =
        if isBaseInput operation.Input2 then
            BaseInput(operation.Input2)
        else
            IntermediateInput(decompose map map[operation.Input2])

    input1, operation.Operator, input2


let isSus (operationTree: OperationTreeNode) =
    


let wires, operations = parseInput "input.dat"
let x = decodeNumber 'x' wires
printfn "x:           %B" x

let y = decodeNumber 'y' wires
printfn "y:           %B" y

let zActual = (wires, operations) ||> applyOperations |> decodeNumber 'z'
printfn "z actual:   %B" zActual

let zExpected = x + y
printfn "z expected: %B" zExpected

printfn "incorrect digits: %i" (countDiffDigits zActual zExpected)


let map = operations |> List.map (fun op -> op.Output, op) |> Map.ofList

printfn "%A" (decompose map {Input1="rpj"; Operator=XOR; Input2="nsc"; Output="z01"})

// prettyPrintCircuit operations

// for op in operations |> List.sortBy (fun op -> op.Input1, op.Input2, op.Output) do
//     if (op.Input1[0] = 'x' && op.Input2[0] = 'y') || (op.Input1[0] = 'y' && op.Input2[0] = 'x')
//     then printfn "%A" op

// for op in operations |> List.sortBy (fun op -> op.Input1, op.Input2) do
//     if ((op.Input1[0] = 'x' && op.Input2[0] = 'y') || (op.Input1[0] = 'y' && op.Input2[0] = 'x')) && op.Operator = XOR
//     then printfn "%A" op

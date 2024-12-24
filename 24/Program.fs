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


let buildNumber (wires: Map<string, bool>) =
    let s =
        wires
        |> Map.toList
        |> List.filter (fun (wireName, _) -> wireName[0] = 'z')
        |> List.sort
        |> List.rev
        |> List.map snd
        |> List.map (fun b -> if b then "1" else "0")
        |> String.concat ""

    Convert.ToInt64(s, 2)


// parseInput "test.dat" |> printfn "%A"
parseInput "input.dat" ||> applyOperations |> buildNumber |> part1

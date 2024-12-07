open Common.Functions

type Equation = { Result: int64; Operands: int64 list }

let parseEquation (str: string) =
    let result = int64 str[.. str.IndexOf ":" - 1]

    let operands =
        str[str.IndexOf ":" + 2 ..] |> _.Split(" ") |> Array.toList |> List.map int64

    { Result = result; Operands = operands }

let possibleResults (operators: (int64 -> int64 -> int64) list) (operands: int64 list) =
    let rec helper (remainingOperands: int64 list) (soFar: int64) =
        match remainingOperands with
        | [] -> Set.ofList [ soFar ]
        | operand :: remaining ->
            operators
            |> List.map (fun operator -> operator soFar operand)
            |> List.map (helper remaining)
            |> List.toSeq
            |> Set.unionMany

    helper operands[1..] operands[0]

let calibrationValue equations operators =
    equations
    |> List.map (fun equation -> possibleResults operators equation.Operands)
    |> List.zip equations
    |> List.filter (fun (equation, results) -> Set.contains equation.Result results)
    |> List.map (fun (equation, _) -> equation.Result)
    |> List.sum

let equations = readlines "input.dat" |> List.map parseEquation
calibrationValue equations [ (+); (*) ] |> part1

calibrationValue equations [ (+); (*); fun n m -> int64 ((string n) + (string m)) ]
|> part2

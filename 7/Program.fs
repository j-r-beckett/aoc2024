open Common.Functions

type Equation = { Result: int64; Operands: int64 list }

let parseEquation (str: string) =
    let result = int64 str[.. str.IndexOf ":" - 1]

    let operands =
        str[str.IndexOf ":" + 2 ..] |> _.Split(" ") |> Array.toList |> List.map int64

    { Result = result; Operands = operands }

let rec listOr f lst =
    match lst with
    | [] -> false
    | head :: tail -> f head || listOr f tail

let isValid (operators: (int64 -> int64 -> int64) list) (equation: Equation) =
    let rec helper (remainingOperands: int64 list) (soFar: int64) =
        if soFar > equation.Result then
            false
        else
            match remainingOperands with
            | [] -> soFar = equation.Result
            | operand :: remaining -> operators |> listOr (fun operator -> helper remaining (operator soFar operand))

    helper equation.Operands[1..] equation.Operands[0]

let calibrationValue equations operators =
    equations
    |> List.filter (isValid operators)
    |> List.map (fun equation -> equation.Result)
    |> List.sum

let equations = readlines "input.dat" |> List.map parseEquation
calibrationValue equations [ (+); (*) ] |> part1

calibrationValue equations [ (+); (*); fun n m -> int64 ((string n) + (string m)) ]
|> part2
